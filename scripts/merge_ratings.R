# Add tournament games to the ratings data (issue #18).
# Run from the project root:  Rscript scripts/merge_ratings.R [--allow-unresolved]
# See README.md, section "Importar resultados de torneos".

source("scripts/tengen_players.R")
source("scripts/import_opengotha.R")
source("scripts/tengen_merge.R")

args <- commandArgs(trailingOnly = TRUE)
allow_unresolved <- "--allow-unresolved" %in% args

ratings_file     <- "data/Club Tengen de Go - Registro de Partidas (Responses).xlsx"
tournaments_file <- "data/Torneos CMM 2026.xlsx"
index_file       <- "outputs/player_index.xlsx"
out_file         <- "outputs/Club Tengen de Go - Registro de Partidas (Responses).xlsx"

if (!file.exists(index_file)) stop("Run scripts/build_player_index.R first.", call. = FALSE)
lookup <- as.data.frame(readxl::read_excel(index_file, sheet = "lookup", col_types = "text"))
index  <- as.data.frame(readxl::read_excel(index_file, sheet = "index"))

# name checks (warnings only)
one_word <- index$recommended_name[!grepl(" ", trimws(index$recommended_name))]
if (length(one_word) > 0) {
  warning(sprintf("%d players have a recommended name without a surname (fill in 'new_name' in sheet 'renames'): %s%s",
                  length(one_word), paste(head(one_word, 15), collapse = ", "),
                  if (length(one_word) > 15) ", ..." else ""), call. = FALSE)
}
rk <- normalize_name(index$recommended_name)
if (anyDuplicated(rk) > 0) {
  warning("Different player_ids share the same recommended name (merge them in sheet 'decisions'): ",
          paste(unique(index$recommended_name[rk %in% rk[duplicated(rk)]]), collapse = ", "), call. = FALSE)
}

ratings <- read_ratings(ratings_file)
games   <- read_all_tournaments(tournaments_file)

# players still waiting for a decision
pending <- index[index$needs_review %in% c(TRUE, "TRUE"), ]
used <- unique(c(games$black_name, games$white_name))
pending <- pending[vapply(pending$tournament_names, function(x) any(strsplit(x, "; ")[[1]] %in% used), logical(1)), ]
if (nrow(pending) > 0 && !allow_unresolved) {
  stop(sprintf(paste0("%d players in the tournament data have possible matches still unresolved ",
                      "(sheet 'decisions' of %s): %s.\nResolve them and re-run build_player_index.R, ",
                      "or use --allow-unresolved to treat them as separate players."),
               nrow(pending), index_file, paste(pending$recommended_name, collapse = ", ")), call. = FALSE)
}

res <- merge_tournament_games(ratings, games, lookup)
old <- apply_recommended_names(ratings, lookup)
unknown <- attr(resolve_names(c(ratings[[3]], ratings[[4]]), "ratings", lookup), "unknown")
if (length(unknown)) {
  warning("Names in the ratings data missing from the index (re-run build_player_index.R): ",
          paste(unknown, collapse = ", "), call. = FALSE)
}
out <- rbind(old, res$new_rows)
out <- out[order(out[[1]]), ]

dir.create("outputs", showWarnings = FALSE)
write_ratings_workbook(out, out_file)
utils::write.csv(res$report, "outputs/tournament_import_report.csv", row.names = FALSE, fileEncoding = "UTF-8")

tab <- table(res$report$status)
cat(sprintf("Wrote %s: %d historical rows + %d new tournament rows.\n", out_file, nrow(old), nrow(res$new_rows)))
print(tab)
cat("Details per game: outputs/tournament_import_report.csv\n")
if ("conflict" %in% names(tab)) cat("WARNING: games flagged 'conflict' were NOT added; check them in the report.\n")
