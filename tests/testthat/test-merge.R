test_that("duplicates are detected via tournament tag, new games get unique timestamps", {
  f <- tempfile(fileext = ".xlsx"); make_workbook(f)
  g <- read_opengotha_sheet(f, "T_19x19"); g$sheet_order <- 1
  ts <- as.POSIXct("2026-01-24 12:00:00", tz = "UTC")
  ratings <- data.frame(ts, NA_character_, "Ana", "Beto", "19 x 19", 0, "Negro", "Torneo Prueba R1",
                        stringsAsFactors = FALSE)
  names(ratings) <- c("Timestamp", "Email", "N", "B", "S", "H", "W", "C")
  dec <- data.frame(source = "tournament", name_variant = c("Perez Ana", "Lopez Beto"),
                    merge_into = c("Ana", "Beto"))
  x <- build_player_index(c("Ana", "Beto"), unique(c(g$black_name, g$white_name)), decisions = dec)
  res <- merge_tournament_games(ratings, g, x$lookup)
  expect_equal(sort(res$report$status), c("duplicate", "new", "new"))
  expect_equal(nrow(res$new_rows), 2)
  expect_false(anyDuplicated(c(res$new_rows[[1]], ratings[[1]])) > 0)
  expect_true(all(grepl("^Torneo Prueba R[23]$", res$new_rows$C)))
  # running again after the index is rebuilt from the merged data adds nothing
  merged <- rbind(ratings, res$new_rows)
  x2 <- build_player_index(c(merged$N, merged$B), unique(c(g$black_name, g$white_name)),
                           prior_lookup = x$lookup, decisions = dec)
  again <- merge_tournament_games(merged, g, x2$lookup)
  expect_equal(nrow(again$new_rows), 0)
})

test_that("a different winner under the same tag is a conflict, not added", {
  f <- tempfile(fileext = ".xlsx"); make_workbook(f)
  g <- read_opengotha_sheet(f, "T_19x19"); g$sheet_order <- 1
  ratings <- data.frame(as.POSIXct("2026-01-10 12:00:00", tz = "UTC"), NA_character_, "Ana", "Beto",
                        "19 x 19", 0, "Blanco", "Torneo Prueba R1", stringsAsFactors = FALSE)
  names(ratings) <- c("Timestamp", "Email", "N", "B", "S", "H", "W", "C")
  dec <- data.frame(source = "tournament", name_variant = c("Perez Ana", "Lopez Beto"),
                    merge_into = c("Ana", "Beto"))
  x <- build_player_index(c("Ana", "Beto"), unique(c(g$black_name, g$white_name)), decisions = dec)
  res <- merge_tournament_games(ratings, g, x$lookup)
  expect_equal(sum(res$report$status == "conflict"), 1)
})
