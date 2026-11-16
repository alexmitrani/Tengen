# Player index functions.
# See README.md, section "Importar resultados de torneos" for how to use them.
#
# NOTE: these files live in ./scripts (not ./R) on purpose: Shiny automatically
# sources every file in ./R when the app starts.
#
# Merging rule (conservative): players are only merged automatically when there
# is strong evidence:
#   (a) the names are identical after removing accents/case/extra spaces,
#   (b) a coded fix applies (NAME_FIXES, taken from the fixes in app.R), or
#   (c) the user wrote the merge in the "decisions" sheet of player_index.xlsx.
# Similar-looking names (e.g. "Cris" / "Cristian Villacura") are NEVER merged
# automatically; at most they are offered as suggestions.

# Coded name fixes, as normalised keys. Same fixes as in app.R
# (Anibal -> Aníbal, Nicolás -> Nico, Panchito -> Francisco).
NAME_FIXES <- c(nicolas = "nico", panchito = "francisco")

normalize_name <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(trimws(x))
  gsub("\\s+", " ", x)
}

name_key <- function(x) {
  k <- normalize_name(x)
  fixed <- NAME_FIXES[k]
  unname(ifelse(is.na(fixed), k, fixed))
}

# "SURNAME FIRSTNAME(S)" -> "Firstname Surname". The first word is the surname.
tournament_to_firstname_surname <- function(x) {
  vapply(strsplit(trimws(x), "\\s+"), function(tok) {
    tok <- stringi::stri_trans_totitle(tok)
    if (length(tok) == 1) tok else paste(c(tok[-1], tok[1]), collapse = " ")
  }, character(1))
}

n_accents <- function(x) {
  nchar(x) - nchar(stringi::stri_trans_general(x, "Latin-ASCII"))
}

next_ids <- function(n, existing) {
  nums <- suppressWarnings(as.integer(sub("^P", "", existing)))
  start <- if (all(is.na(nums))) 0L else max(nums, na.rm = TRUE)
  sprintf("P%03d", start + seq_len(n))
}

.suggest_for <- function(i, groups) {
  g <- groups[i, ]
  if (g$has_ratings || !g$has_tournament) return("")
  toks <- strsplit(g$key, " ")[[1]]
  out <- character()
  for (j in seq_len(nrow(groups))[-i]) {
    o <- groups[j, ]
    otoks <- strsplit(o$key, " ")[[1]]
    close <- stringdist::stringdist(g$key, o$key, method = "jw") < 0.1
    subset <- all(otoks %in% toks) || all(toks %in% otoks)
    if (o$has_ratings && (subset || close)) {
      out <- c(out, sprintf("%s (ratings)", o$recommended))
    } else if (!o$has_ratings && (close || (subset && length(otoks) != length(toks)))) {
      out <- c(out, sprintf("%s (tournament only)", o$recommended))
    }
  }
  paste(unique(out), collapse = "; ")
}

# Build the player index.
#  ratings_names:    character vector with one element per use of a name in the
#                    ratings data (so frequencies can be counted)
#  tournament_names: distinct names as they appear in the tournament sheets
#  prior_lookup:     previous lookup (source, name_variant, player_id) to keep ids stable
#  decisions:        previous decisions sheet (source, name_variant, merge_into, ...)
#  renames:          previous rename sheet (player_id, new_name)
build_player_index <- function(ratings_names, tournament_names,
                               prior_lookup = NULL, decisions = NULL, renames = NULL) {
  ratings_names <- trimws(ratings_names[!is.na(ratings_names)])
  counts <- table(ratings_names)
  r_vars <- data.frame(source = "ratings", name_variant = names(counts),
                       n = as.integer(counts), stringsAsFactors = FALSE)
  r_vars$full <- r_vars$name_variant
  t_names <- sort(unique(trimws(tournament_names[!is.na(tournament_names)])))
  t_vars <- data.frame(source = "tournament", name_variant = t_names, n = 0L,
                       stringsAsFactors = FALSE)
  t_vars$full <- tournament_to_firstname_surname(t_names)
  vars <- rbind(r_vars, t_vars)
  vars$gkey <- name_key(vars$full)

  # (c) decisions written by the user
  if (!is.null(decisions) && nrow(decisions) > 0) {
    dec <- decisions[!is.na(decisions$merge_into) & trimws(decisions$merge_into) != "", , drop = FALSE]
    for (k in seq_len(nrow(dec))) {
      from <- vars$gkey[vars$source == dec$source[k] & vars$name_variant == dec$name_variant[k]]
      target <- trimws(dec$merge_into[k])
      to <- NA_character_
      if (grepl("^P[0-9]+$", target) && !is.null(prior_lookup)) {
        pv <- prior_lookup[prior_lookup$player_id == target, ]
        hit <- vars$gkey[match(paste(pv$source, pv$name_variant),
                               paste(vars$source, vars$name_variant))]
        if (length(hit) && !all(is.na(hit))) to <- hit[!is.na(hit)][1]
      }
      if (is.na(to)) {
        tk <- name_key(target)
        if (tk %in% vars$gkey) to <- tk
        else {
          tk2 <- name_key(tournament_to_firstname_surname(target))
          if (tk2 %in% vars$gkey) to <- tk2
        }
      }
      if (length(from) == 0 || is.na(to)) {
        warning(sprintf("Decision not applied (name or target not found): '%s' -> '%s'",
                        dec$name_variant[k], target), call. = FALSE)
        next
      }
      vars$gkey[vars$gkey == from[1]] <- to
    }
  }

  # ids
  gkeys <- unique(vars$gkey)
  prior_id <- rep(NA_character_, nrow(vars))
  if (!is.null(prior_lookup) && nrow(prior_lookup) > 0) {
    prior_id <- prior_lookup$player_id[match(paste(vars$source, vars$name_variant),
                                             paste(prior_lookup$source, prior_lookup$name_variant))]
  }
  ids <- setNames(rep(NA_character_, length(gkeys)), gkeys)
  for (g in gkeys) {
    p <- prior_id[vars$gkey == g]
    p <- p[!is.na(p)]
    if (length(p)) ids[g] <- sort(p)[1]
  }
  missing <- is.na(ids)
  ids[missing] <- next_ids(sum(missing), c(ids[!missing], prior_lookup$player_id))
  vars$player_id <- unname(ids[vars$gkey])

  # recommended names
  rec <- vapply(gkeys, function(g) {
    v <- vars[vars$gkey == g, ]
    tv <- v[v$source == "tournament", ]
    if (nrow(tv)) {
      tv <- tv[order(-n_accents(tv$full), tv$full), ]
      return(tv$full[1])
    }
    is_fix <- normalize_name(v$name_variant) %in% names(NAME_FIXES)
    v <- v[order(is_fix, -v$n, -n_accents(v$name_variant)), ]
    v$name_variant[1]
  }, character(1))
  vars$recommended <- unname(rec[vars$gkey])
  if (!is.null(renames) && nrow(renames) > 0) {
    ren <- renames[!is.na(renames$new_name) & trimws(renames$new_name) != "", ]
    for (k in seq_len(nrow(ren))) {
      vars$recommended[vars$player_id == ren$player_id[k]] <- trimws(ren$new_name[k])
    }
  }

  # one row per player
  groups <- do.call(rbind, lapply(gkeys, function(g) {
    v <- vars[vars$gkey == g, ]
    data.frame(
      key = g, player_id = v$player_id[1], recommended = v$recommended[1],
      has_ratings = any(v$source == "ratings"), has_tournament = any(v$source == "tournament"),
      ratings_names = paste(v$name_variant[v$source == "ratings"], collapse = "; "),
      tournament_names = paste(v$name_variant[v$source == "tournament"], collapse = "; "),
      n_games_ratings = sum(v$n), stringsAsFactors = FALSE)
  }))
  groups <- groups[order(groups$player_id), ]
  groups$suggested_matches <- vapply(seq_len(nrow(groups)), .suggest_for, character(1), groups = groups)
  groups$status <- ifelse(groups$has_ratings & groups$has_tournament, "linked",
                   ifelse(groups$has_ratings, "ratings_only", "tournament_only"))
  groups$needs_review <- groups$suggested_matches != ""
  groups$index <- seq_len(nrow(groups))

  index <- groups[, c("index", "player_id", "recommended", "ratings_names",
                      "tournament_names", "status", "needs_review", "n_games_ratings")]
  names(index)[3] <- "recommended_name"

  lookup <- vars[, c("source", "name_variant", "player_id", "recommended")]
  names(lookup)[4] <- "recommended_name"

  # decisions sheet: pending rows + decisions already applied
  pending <- groups[groups$needs_review, ]
  new_dec <- data.frame(
    source = rep("tournament", nrow(pending)),
    name_variant = vapply(pending$tournament_names, function(x) strsplit(x, "; ")[[1]][1], character(1)),
    player_id = pending$player_id, recommended_name = pending$recommended,
    suggested_matches = pending$suggested_matches, merge_into = rep("", nrow(pending)), stringsAsFactors = FALSE)
  if (!is.null(decisions) && nrow(decisions) > 0) {
    kept <- decisions[!is.na(decisions$merge_into) & trimws(decisions$merge_into) != "", , drop = FALSE]
    kept <- kept[, intersect(names(new_dec), names(kept)), drop = FALSE]
    for (cn in setdiff(names(new_dec), names(kept))) kept[[cn]] <- ""
    kept <- kept[, names(new_dec)]
    new_dec <- rbind(new_dec[!new_dec$name_variant %in% kept$name_variant, ], kept)
  }
  if (is.null(renames) || nrow(renames) == 0) {
    renames <- data.frame(player_id = character(), new_name = character(), stringsAsFactors = FALSE)
  }
  list(index = index, lookup = lookup, decisions = new_dec, renames = renames)
}

# Map names to recommended names. Unknown names are returned unchanged and
# reported in attr(, "unknown").
resolve_names <- function(x, source, lookup, what = "recommended_name") {
  l <- lookup[lookup$source == source, ]
  i <- match(trimws(x), l$name_variant)
  out <- l[[what]][i]
  unknown <- unique(x[is.na(i) & !is.na(x)])
  out[is.na(i)] <- if (what == "recommended_name") x[is.na(i)] else NA_character_
  attr(out, "unknown") <- unknown
  out
}
