test_that("result cells are parsed", {
  p <- parse_result_cell("12+/b0")
  expect_equal(c(p$opponent, p$result, p$colour, p$handicap), c(12, "+", "b", 0))
  p <- parse_result_cell("4-")
  expect_true(is.na(p$colour)); expect_equal(p$result, "-")
  expect_equal(parse_result_cell("0+")$opponent, 0)
  expect_null(parse_result_cell(NA)); expect_null(parse_result_cell(""))
  expect_error(parse_result_cell("xyz"), "Unrecognised")
  expect_error(parse_result_cell("3="), "Draws")
})

test_that("sheet is read: one row per game, byes skipped, colours and winners", {
  f <- tempfile(fileext = ".xlsx"); make_workbook(f)
  g <- read_opengotha_sheet(f, "T_19x19")
  expect_equal(nrow(g), 3)                       # 1v2 R1, 1v3 R2, 2v3 R3; byes skipped
  expect_equal(g$date[1], as.Date("2026-01-24"))
  r1 <- g[g$round == 1, ]                        # 1 beat 2 with black
  expect_equal(c(r1$black_name, r1$white_name, r1$winner), c("Perez Ana", "Lopez Beto", "Negro"))
  r2 <- g[g$round == 2, ]                        # colour unknown: 3 won -> assumed white
  expect_equal(c(r2$white_name, r2$winner, r2$colour_known), c("Soto Cris", "Blanco", FALSE))
  expect_true(all(g$board == "19 x 19"))
  g9 <- read_opengotha_sheet(f, "T_9x9")         # text date and board from sheet name
  expect_equal(g9$date[1], as.Date("2026-01-24")); expect_true(all(g9$board == "9 x 9"))
})

test_that("missing date gives a clear error", {
  f <- tempfile(fileext = ".xlsx"); make_workbook(f)
  expect_error(read_opengotha_sheet(f, "NoDate"), "date not found")
})
