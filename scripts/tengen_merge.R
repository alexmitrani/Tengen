# Merge tournament games into the ratings data.
# See README.md, section "Importar resultados de torneos".

RATINGS_SHEET <- "Form responses 1"

read_ratings <- function(path) {
  d <- readxl::read_excel(path, sheet = RATINGS_SHEET)
  if (ncol(d) != 8) stop("Unexpected ratings workbook: expected 8 columns", call. = FALSE)
  d
}

tournament_tag <- function(title) {
  if (grepl("^torneo", normalize_name(title))) title else paste("Torneo", title)
}

# ratings: data frame with the 8 original columns (names resolved to recommended names
#          is NOT required: ids are looked up from the raw names)
# games:   output of read_all_tournaments()
# Returns list(new_rows, duplicates, conflicts), new_rows having the ratings layout.
merge_tournament_games <- function(ratings, games, lookup) {
  id_of <- function(x, source) {
    l <- lookup[lookup$source == source, ]
    l$player_id[match(trimws(x), l$name_variant)]
  }
  r_black <- id_of(ratings[[3]], "ratings"); r_white <- id_of(ratings[[4]], "ratings")
  r_winner <- ifelse(ratings[[7]] == "Negro", r_black, r_white)
  r_comment <- normalize_name(ifelse(is.na(ratings[[8]]), "", ratings[[8]]))
  r_day <- as.Date(ratings[[1]])
  used <- rep(FALSE, nrow(ratings))

  g_black <- id_of(games$black_name, "tournament"); g_white <- id_of(games$white_name, "tournament")
  if (anyNA(c(g_black, g_white))) {
    stop("Tournament names missing from the player index; re-run scripts/build_player_index.R: ",
         paste(unique(c(games$black_name[is.na(g_black)], games$white_name[is.na(g_white)])), collapse = ", "),
         call. = FALSE)
  }
  g_winner <- ifelse(games$winner == "Negro", g_black, g_white)
  g_black_name <- resolve_names(games$black_name, "tournament", lookup)
  g_white_name <- resolve_names(games$white_name, "tournament", lookup)

  status <- character(nrow(games)); reason <- character(nrow(games)); match_row <- integer(nrow(games))
  for (i in seq_len(nrow(games))) {
    same_pair <- !is.na(r_black) & !is.na(r_white) & !used &
      ((r_black == g_black[i] & r_white == g_white[i]) | (r_black == g_white[i] & r_white == g_black[i]))
    tkey <- normalize_name(games$title[i])
    has_title <- grepl(tkey, r_comment, fixed = TRUE)
    has_round <- grepl(paste0("\\br", games$round[i], "\\b"), r_comment)
    no_round <- !grepl("\\br[0-9]+\\b", r_comment)
    tag <- has_title & (has_round | no_round)
    same_day <- r_day == games$date[i]
    same_winner <- !is.na(r_winner) & r_winner == g_winner[i]
    cand <- which(same_pair & same_winner & (tag | same_day))
    conf <- which(same_pair & !same_winner & tag)
    if (length(cand)) {
      k <- cand[order(!tag[cand])][1]
      status[i] <- "duplicate"; reason[i] <- if (tag[k]) "same tournament tag" else "same pair, winner and day"
      match_row[i] <- k; used[k] <- TRUE
    } else if (length(conf)) {
      status[i] <- "conflict"; reason[i] <- "same pair and tournament tag but different winner"
      match_row[i] <- conf[1]; used[conf[1]] <- TRUE
    } else status[i] <- "new"
  }

  report <- data.frame(sheet = games$sheet, round = games$round, date = games$date,
                       black = g_black_name, white = g_white_name, winner = games$winner,
                       status = status, reason = reason,
                       existing_row = ifelse(match_row == 0, NA, match_row + 1L), # +1: header row in sheet
                       stringsAsFactors = FALSE)

  games$black_rec <- g_black_name; games$white_rec <- g_white_name
  new <- games[status == "new", ]
  new <- new[order(new$date, new$sheet_order, new$round, new$black_num), ]
  if (nrow(new) == 0) return(list(new_rows = ratings[0, ], report = report))
  nb <- new$black_rec; nw <- new$white_rec
  ts <-as.POSIXct(paste(new$date, "12:00:00"), tz = "UTC")
  ts <- ts + 30 * (ave(seq_along(ts), new$date, FUN = seq_along) - 1)
  for (k in seq_along(ts)) {                                # keep timestamps unique
    while (ts[k] %in% c(ratings[[1]], ts[seq_len(k - 1)])) ts[k] <- ts[k] + 1
  }
  new_rows <- ratings[0, ]
  if (nrow(new) > 0) {
    new_rows <- data.frame(ts, NA_character_, nb, nw, new$board, as.numeric(new$handicap), new$winner,
                           paste0(vapply(new$title, tournament_tag, character(1)), " R", new$round),
                           stringsAsFactors = FALSE)
    names(new_rows) <- names(ratings)
    new_rows[[1]] <- ts
  }
  list(new_rows = new_rows, report = report)
}

# Replace names in the 8-column ratings data by recommended names.
apply_recommended_names <- function(ratings, lookup) {
  for (j in 3:4) {
    r <- resolve_names(ratings[[j]], "ratings", lookup)
    ratings[[j]] <- as.character(r)
  }
  ratings
}

write_ratings_workbook <- function(df, path) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, RATINGS_SHEET)
  out <- df
  out[[1]] <- as.numeric(difftime(out[[1]], as.POSIXct("1899-12-30", tz = "UTC"), units = "days"))
  openxlsx::writeData(wb, RATINGS_SHEET, out)
  openxlsx::addStyle(wb, RATINGS_SHEET,
                     openxlsx::createStyle(numFmt = "dd/mm/yyyy hh:mm:ss"),
                     rows = seq_len(nrow(out)) + 1, cols = 1, gridExpand = TRUE)
  openxlsx::setColWidths(wb, RATINGS_SHEET, cols = 1:8, widths = c(20, 25, 25, 25, 12, 10, 12, 45))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}
