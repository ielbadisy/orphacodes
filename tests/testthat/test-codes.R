test_that("orphacode normalises every accepted format", {
  expect_equal(orphacode(c("ORPHA:324", "orpha 324", "ORPHA324", " 0324 ", 324, "Q78.0", NA)),
               c("324", "324", "324", "324", "324", NA, NA))
  expect_equal(orphacode("324", prefix = TRUE), "ORPHA:324")
  expect_equal(orphacode(c(1.5, -1)), c(NA_character_, NA_character_))
})

test_that("orphalookup and orphavalidate agree on status", {
  l <- orphalookup(c("ORPHA:324", "166068", "1"), db = orphamini)
  expect_s3_class(l, "basetable")
  expect_equal(l$found, c(TRUE, TRUE, FALSE))
  expect_equal(l$name[1], "Fabry disease")
  v <- orphavalidate(c("ORPHA:324", "166068", "abc", NA, "1"), db = orphamini)
  expect_equal(v$issue, c("ok", "deprecated", "invalid_format", "missing", "unknown"))
  expect_equal(v$replacement[2], "166063")
  expect_equal(v$relation[2], "Moved to")
})

test_that("orphasearch finds names and synonyms", {
  s <- orphasearch("fabry", db = orphamini)
  expect_equal(s$orphacode[1], "324")
  expect_equal(nrow(orphasearch("zzzzqq", db = orphamini)), 0L)
  expect_true(nrow(orphasearch("^Fabry", db = orphamini, regex = TRUE)) >= 1)
})

test_that("orphastandardize matches exact, synonym and fuzzy tiers", {
  reg <- data.frame(dx = c("Fabry disease", "Anderson-Fabry disease", "Fabry desease", "Flu", NA))
  r <- orphastandardize(reg, disease = "dx", db = orphamini)
  expect_equal(r$orphacode, c("324", "324", "324", NA, NA))
  expect_equal(r$orphacode_match, c("exact", "synonym", "fuzzy", "none", "none"))
  expect_true(r$orphacode_score[3] < 1)
  r2 <- orphastandardize(reg, disease = "dx", db = orphamini, fuzzy = FALSE)
  expect_true(is.na(r2$orphacode[3]))
})

test_that("a swap of two adjacent letters counts as one edit", {
  # two edits under plain Levenshtein (2/16 > 0.1), one with the swap rule
  r <- orphastandardize(data.frame(dx = "Sandhfof disease"), disease = "dx", db = orphamini)
  expect_equal(r$orphacode_name, "Sandhoff disease")
  expect_equal(r$orphacode_match, "fuzzy")
  expect_gt(r$orphacode_score, 0.9)
})

test_that("max_distance sets how loose a fuzzy match may be", {
  reg <- data.frame(dx = "Fabri desease")
  expect_equal(orphastandardize(reg, disease = "dx", db = orphamini)$orphacode_match, "none")
  r <- orphastandardize(reg, disease = "dx", db = orphamini, max_distance = 0.2)
  expect_equal(r$orphacode, "324")
})

test_that("fuzzy matching handles word order, French names and short names", {
  reg <- data.frame(dx = c("Fabry desease", "Disease, Fabry", "Maladie de Fabry",
                           "syndrome de Dubowitz", "Dubowitz syndrom", "Flu"))
  r <- orphastandardize(reg, disease = "dx", db = orphamini)
  expect_equal(r$orphacode[1:5], c("324", "324", "324", "235", "235"))
  expect_equal(r$orphacode_match[6], "none")
  # a short word never fuzzy-matches
  r <- orphastandardize(data.frame(dx = "Fabr"), disease = "dx", db = orphamini)
  expect_equal(r$orphacode_match, "none")
})

test_that("subset hint is given once for unknown codes", {
  withr::local_options(rlib_message_verbosity = "verbose")
  expect_message(orphalookup("99999999", db = orphamini), "bundled subset")
})

test_that("British spellings and possessives match exactly", {
  reg <- data.frame(dx = c("Sickle cell anaemia", "Diamond-Blackfan anaemia",
                           "Galactosaemia", "Fabry's disease"))
  r <- orphastandardize(reg, disease = "dx", db = orphamini)
  expect_equal(r$orphacode_name[1:3], c("Sickle cell anemia", "Diamond-Blackfan anemia", "Galactosemia"))
  expect_equal(r$orphacode_match[1:3], rep("exact", 3))
  expect_false(is.na(r$orphacode[4]))
})

test_that("qualifiers such as familial or hereditary are not ignored", {
  r <- orphastandardize(data.frame(dx = c("dysautonomia", "orotic aciduria")),
                        disease = "dx", db = orphamini)
  expect_false(any(r$orphacode_name %in% c("Familial dysautonomia", "Hereditary orotic aciduria") &
                     r$orphacode_score == 1, na.rm = TRUE))
})

test_that("returned tables are built without deprecation warnings", {
  expect_no_warning(l <- orphalookup(c("ORPHA:324", "1"), db = orphamini))
  expect_s3_class(l, "basetable")
  expect_no_warning(orphavalidate(c("ORPHA:324", "abc"), db = orphamini))
})
