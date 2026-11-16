# Build / update the player index (issue #19).
# Run from the project root:  Rscript scripts/build_player_index.R
# See README.md, section "Importar resultados de torneos".

source("scripts/tengen_players.R")
source("scripts/import_opengotha.R")
source("scripts/tengen_merge.R")

ratings_file    <- "data/Club Tengen de Go - Registro de Partidas (Responses).xlsx"
tournaments_file <- "data/Torneos CMM 2026.xlsx"
index_file      <- "outputs/player_index.xlsx"

ratings <- read_ratings(ratings_file)
t_names <- tournament_names_in(tournaments_file)

# keep earlier ids and the user's decisions / renames if the index already exists
prior <- list(lookup = NULL, decisions = NULL, renames = NULL)
if (file.exists(index_file)) {
  sheets <- readxl::excel_sheets(index_file)
  rd <- function(s) as.data.frame(readxl::read_excel(index_file, sheet = s, col_types = "text"))
  if ("lookup" %in% sheets) prior$lookup <- rd("lookup")
  if ("decisions" %in% sheets) prior$decisions <- rd("decisions")
  if ("renames" %in% sheets) prior$renames <- rd("renames")
}

res <- build_player_index(c(ratings[[3]], ratings[[4]]), t_names,
                          prior$lookup, prior$decisions, prior$renames)

dir.create("outputs", showWarnings = FALSE)
wb <- openxlsx::createWorkbook()
for (s in c("index", "decisions", "renames", "lookup")) {
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, res[[s]])
  openxlsx::setColWidths(wb, s, cols = seq_len(ncol(res[[s]])), widths = "auto")
  openxlsx::freezePane(wb, s, firstRow = TRUE)
}
openxlsx::saveWorkbook(wb, index_file, overwrite = TRUE)

n_review <- sum(res$index$needs_review)
cat(sprintf("Player index written to %s: %d players (%d linked, %d ratings only, %d tournament only).\n",
            index_file, nrow(res$index), sum(res$index$status == "linked"),
            sum(res$index$status == "ratings_only"), sum(res$index$status == "tournament_only")))
if (n_review > 0) {
  cat(sprintf("%d tournament players have possible matches to review: open sheet 'decisions', fill 'merge_into' where they are the same person, then re-run this script.\n", n_review))
}
