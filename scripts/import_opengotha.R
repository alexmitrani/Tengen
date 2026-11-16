# OpenGotha tournament workbook importer.
# See README.md, section "Importar resultados de torneos".
#
# Expected layout of every sheet: row 1 = tournament title; a header row whose
# first cell is "Num" (columns Num, Name, "R 1", "R 2", ...); one row per player;
# below the table a cell starting with "OpenGotha" and, directly below it, the
# tournament date (Excel date/time or text like "24/01/2026 18:32:00").

# Parse one result cell, e.g. "12+", "4-/w0", "0+" (bye). Returns NULL for empty.
parse_result_cell <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(NULL)
  m <- regmatches(x, regexec("^(\\d+)([+=-])(?:/([bw])(\\d+))?$", x))[[1]]
  if (length(m) == 0) stop(sprintf("Unrecognised result cell: '%s'", x), call. = FALSE)
  if (m[3] == "=") stop(sprintf("Draws are not supported (cell '%s')", x), call. = FALSE)
  list(opponent = as.integer(m[2]), result = m[3],
       colour = if (m[4] == "") NA_character_ else m[4],
       handicap = if (m[5] == "") NA_integer_ else as.integer(m[5]))
}

# Date from the cell below the "OpenGotha" cell. Returns a Date.
find_opengotha_date <- function(raw) {
  hit <- which(grepl("^OpenGotha", raw))
  if (length(hit) == 0) return(as.Date(NA))
  pos <- arrayInd(hit[1], dim(raw))
  if (pos[1, 1] + 1 > nrow(raw)) return(as.Date(NA))
  v <- raw[pos[1, 1] + 1, pos[1, 2]]
  if (is.na(v)) return(as.Date(NA))
  num <- suppressWarnings(as.numeric(v))
  if (!is.na(num)) return(as.Date(floor(num), origin = "1899-12-30"))
  as.Date(as.POSIXct(v, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"))
}

detect_board <- function(...) {
  for (s in c(...)) {
    m <- regmatches(s, regexec("(9|13|19) ?x ?(9|13|19)", s))[[1]]
    if (length(m) && m[2] == m[3]) return(sprintf("%s x %s", m[2], m[2]))
  }
  "19 x 19"
}

# One tidy row per game for one sheet.
read_opengotha_sheet <- function(path, sheet) {
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                             col_types = "text", .name_repair = "minimal"))
  raw <- as.matrix(raw)
  title <- raw[1, 1]
  hdr <- which(raw[, 1] == "Num")[1]
  if (is.na(hdr)) stop(sprintf("Sheet '%s': header row (first cell 'Num') not found", sheet), call. = FALSE)
  header <- raw[hdr, ]
  name_col <- which(header == "Name")[1]
  r_cols <- which(grepl("^R ?[0-9]+$", header))
  if (is.na(name_col) || length(r_cols) == 0) {
    stop(sprintf("Sheet '%s': columns 'Name' and 'R 1'... not found", sheet), call. = FALSE)
  }
  date <- find_opengotha_date(raw)
  if (is.na(date)) {
    stop(sprintf("Sheet '%s': tournament date not found (expected below the 'OpenGotha' cell)", sheet),
         call. = FALSE)
  }
  rows <- integer()
  r <- hdr + 1
  while (r <= nrow(raw) && !is.na(suppressWarnings(as.numeric(raw[r, 1])))) { rows <- c(rows, r); r <- r + 1 }
  players <- data.frame(num = as.integer(as.numeric(raw[rows, 1])), name = trimws(raw[rows, name_col]),
                        stringsAsFactors = FALSE)

  ent <- list()
  for (k in seq_along(rows)) for (cc in r_cols) {
    p <- parse_result_cell(raw[rows[k], cc])
    if (is.null(p) || p$opponent == 0) next   # empty cell or bye
    rnd <- as.integer(sub("^R ?", "", header[cc]))
    ent[[length(ent) + 1]] <- data.frame(num = players$num[k], round = rnd, opp = p$opponent,
                                         result = p$result, colour = p$colour, handicap = p$handicap,
                                         stringsAsFactors = FALSE)
  }
  if (length(ent) == 0) return(NULL)
  ent <- do.call(rbind, ent)
  ent$lo <- pmin(ent$num, ent$opp); ent$hi <- pmax(ent$num, ent$opp)
  ent$key <- paste(ent$round, ent$lo, ent$hi)
  ent <- ent[order(ent$key, ent$num != ent$lo), ]   # lower-numbered player's entry first

  board <- detect_board(sheet, title)
  games <- list()
  for (key in unique(ent$key)) {
    e <- ent[ent$key == key, ]
    a <- e[1, ]
    if (nrow(e) > 1) {
      b <- e[2, ]
      if (a$result == b$result) {
        warning(sprintf("Sheet '%s' round %d: players %d and %d have inconsistent results", sheet, a$round, a$lo, a$hi), call. = FALSE)
      }
    } else {
      warning(sprintf("Sheet '%s' round %d: game %d-%d has no reciprocal cell", sheet, a$round, a$lo, a$hi), call. = FALSE)
      b <- NULL
    }
    lo_wins <- if (a$num == a$lo) a$result == "+" else a$result == "-"
    # colour of the lower-numbered player ("b"/"w"/NA)
    col_lo <- NA_character_
    for (x in list(a, b)) if (!is.null(x) && is.na(col_lo) && !is.na(x$colour)) {
      col_lo <- if (x$num == x$lo) x$colour else c(b = "w", w = "b")[[x$colour]]
    }
    colour_known <- !is.na(col_lo)
    lo_black <- if (colour_known) col_lo == "b" else !lo_wins   # unknown: winner assumed white
    hc <- c(a$handicap, if (!is.null(b)) b$handicap)
    hc <- if (all(is.na(hc))) 0L else hc[!is.na(hc)][1]
    black <- if (lo_black) a$lo else a$hi
    white <- if (lo_black) a$hi else a$lo
    black_wins <- (black == a$lo) == lo_wins
    games[[length(games) + 1]] <- data.frame(
      sheet = sheet, title = title, date = date, round = a$round,
      black_num = black, white_num = white,
      black_name = players$name[match(black, players$num)],
      white_name = players$name[match(white, players$num)],
      winner = if (black_wins) "Negro" else "Blanco", board = board,
      handicap = hc, colour_known = colour_known, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, games)
  out[order(out$round, out$black_num), ]
}

read_all_tournaments <- function(path, sheets = NULL) {
  if (is.null(sheets)) sheets <- readxl::excel_sheets(path)
  res <- lapply(sheets, read_opengotha_sheet, path = path)
  names(res) <- sheets
  res <- res[!vapply(res, is.null, logical(1))]
  out <- do.call(rbind, res)
  out$sheet_order <- match(out$sheet, sheets)
  rownames(out) <- NULL
  out
}

tournament_names_in <- function(path) {
  raw_names <- lapply(readxl::excel_sheets(path), function(s) {
    raw <- suppressMessages(readxl::read_excel(path, sheet = s, col_names = FALSE,
                                               col_types = "text", .name_repair = "minimal"))
    raw <- as.matrix(raw)
    hdr <- which(raw[, 1] == "Num")[1]
    nc <- which(raw[hdr, ] == "Name")[1]
    nm <- raw[(hdr + 1):nrow(raw), c(1, nc)]
    nm <- nm[!is.na(suppressWarnings(as.numeric(nm[, 1]))), 2]
    trimws(nm[!is.na(nm)])
  })
  unique(unlist(raw_names))
}
