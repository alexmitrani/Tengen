root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
if (!file.exists(file.path(root, "scripts", "tengen_players.R"))) root <- getwd()
for (f in c("tengen_players.R", "import_opengotha.R", "tengen_merge.R")) {
  source(file.path(root, "scripts", f), local = FALSE)
}

# Small OpenGotha-like workbook in a temp file
make_workbook <- function(path) {
  s1 <- rbind(
    c("Torneo Prueba", rep(NA, 8)),
    c("Num", "Name", "R 1", "R 2", "R 3", NA, NA, NA, NA),
    c("1", "Perez Ana", "2+/b0", "3-", "0+", NA, NA, NA, NA),
    c("2", "Lopez Beto", "1-/w0", "0-", "3+", NA, NA, NA, NA),
    c("3", "Soto Cris", "0+", "1+", "2-", NA, NA, NA, NA),
    c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
    c("OpenGotha 3.50", rep(NA, 8)),
    c("46046.77", rep(NA, 8)))
  s2 <- s1; s2[1, 1] <- "Torneo Prueba 9x9"; s2[8, 1] <- "24/01/2026 18:32:00"
  s3 <- s1[1:6, ]
  wb <- openxlsx::createWorkbook()
  for (nm in c("T_19x19", "T_9x9", "NoDate")) openxlsx::addWorksheet(wb, nm)
  openxlsx::writeData(wb, "T_19x19", as.data.frame(s1), colNames = FALSE)
  openxlsx::writeData(wb, "T_9x9", as.data.frame(s2), colNames = FALSE)
  openxlsx::writeData(wb, "NoDate", as.data.frame(s3), colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}
