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
  reg <- data.frame(dx = c("Fabry disease", "Anderson-Fabry disease", "Fabri desease", "Flu", NA))
  r <- orphastandardize(reg, disease = "dx", db = orphamini)
  expect_equal(r$orphacode, c("324", "324", "324", NA, NA))
  expect_equal(r$orphacode_match, c("exact", "synonym", "fuzzy", "none", "none"))
  expect_true(r$orphacode_score[3] < 1)
  r2 <- orphastandardize(reg, disease = "dx", db = orphamini, fuzzy = FALSE)
  expect_true(is.na(r2$orphacode[3]))
})
