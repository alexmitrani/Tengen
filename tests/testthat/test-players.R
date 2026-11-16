test_that("normalisation folds accents and case", {
  expect_equal(normalize_name("  Álvaro  "), "alvaro")
  expect_equal(name_key("Nicolás"), "nico"); expect_equal(name_key("Panchito"), "francisco")
})

test_that("only strong evidence merges players; similar names stay separate", {
  r <- c("Alvaro", "Álvaro", "Álvaro", "Cris", "Cristian Villacura", "Felipe", "Felipe Bustos", "Alex")
  t <- c("Villacura Cristian", "Bustos Felipe", "Mitrani Alex", "Kezuka Akiko")
  x <- build_player_index(r, t)
  l <- x$lookup
  id <- function(n, s) l$player_id[l$source == s & l$name_variant == n]
  expect_equal(id("Alvaro", "ratings"), id("Álvaro", "ratings"))                 # accent fold
  expect_equal(id("Villacura Cristian", "tournament"), id("Cristian Villacura", "ratings"))
  expect_equal(id("Bustos Felipe", "tournament"), id("Felipe Bustos", "ratings"))
  expect_false(id("Cris", "ratings") == id("Cristian Villacura", "ratings"))     # NOT merged
  expect_false(id("Felipe", "ratings") == id("Felipe Bustos", "ratings"))
  expect_false(id("Mitrani Alex", "tournament") == id("Alex", "ratings"))        # needs a decision
  expect_true(x$index$needs_review[x$index$recommended_name == "Alex Mitrani"])
  expect_equal(length(unique(l$player_id)), nrow(x$index))
})

test_that("decisions merge players and ids stay stable", {
  r <- c("Alex", "Cris"); t <- c("Mitrani Alex")
  x1 <- build_player_index(r, t)
  dec <- data.frame(source = "tournament", name_variant = "Mitrani Alex", merge_into = "Alex")
  x2 <- build_player_index(r, t, prior_lookup = x1$lookup, decisions = dec)
  l <- x2$lookup
  expect_equal(l$player_id[l$name_variant == "Mitrani Alex"], l$player_id[l$name_variant == "Alex"])
  expect_equal(x2$lookup$recommended_name[l$name_variant == "Alex"], "Alex Mitrani")
  expect_equal(l$player_id[l$name_variant == "Cris"], x1$lookup$player_id[x1$lookup$name_variant == "Cris"])
  # decision is carried over on rebuild
  x3 <- build_player_index(r, t, prior_lookup = x2$lookup, decisions = x2$decisions)
  expect_equal(x3$index$player_id, x2$index$player_id)
})

test_that("decisions sheet gives the player_id of each suggested match", {
  x <- build_player_index(c("Alex", "Cris"), c("Mitrani Alex"))
  d <- x$decisions
  alex_id <- x$lookup$player_id[x$lookup$source == "ratings" & x$lookup$name_variant == "Alex"]
  expect_equal(d$suggested_match_player_id[d$name_variant == "Mitrani Alex"], alex_id)
  # using the suggested id as the decision merges the players
  d$merge_into <- d$suggested_match_player_id
  x2 <- build_player_index(c("Alex", "Cris"), c("Mitrani Alex"), prior_lookup = x$lookup, decisions = d)
  expect_equal(nrow(x2$index), 2)
})

test_that("accented spelling is preferred for the recommended name", {
  expect_equal(n_accents(c("Nicolas", "Nicolás")), c(0, 1))
  x <- build_player_index("Alex", c("Varas Nicolas", "Varas Nicolás"))
  expect_true("Nicolás Varas" %in% x$index$recommended_name)
})

test_that("players without a surname are flagged and listed in the renames template", {
  x <- build_player_index(c("Alex", "Cris", "Cris"), c("Mitrani Alex"),
                          decisions = data.frame(source = "tournament", name_variant = "Mitrani Alex", merge_into = "Alex"))
  expect_equal(x$index$needs_surname[x$index$recommended_name == "Cris"], TRUE)
  expect_equal(x$index$needs_surname[x$index$recommended_name == "Alex Mitrani"], FALSE)
  expect_equal(x$renames$current_name, "Cris"); expect_true(is.na(x$renames$new_name))
  # filling in new_name applies it and keeps the row on rebuild
  ren <- data.frame(player_id = x$renames$player_id, new_name = "Cris Rojas")
  x2 <- build_player_index(c("Alex", "Cris", "Cris"), c("Mitrani Alex"), prior_lookup = x$lookup,
                           decisions = data.frame(source = "tournament", name_variant = "Mitrani Alex", merge_into = "Alex"),
                           renames = ren)
  expect_true("Cris Rojas" %in% x2$index$recommended_name)
  expect_false(any(x2$index$needs_surname))
  expect_equal(x2$renames$new_name, "Cris Rojas")
})
