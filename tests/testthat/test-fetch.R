test_that("orphacache honours the option", {
  d <- tempfile()
  withr::local_options(orphacodes.cache_dir = d)
  expect_equal(orphacache(create = FALSE), d)
  expect_false(dir.exists(d))
  expect_true(dir.exists(orphacache()))
})

test_that("orphaversions and orphafetch work online", {
  skip_on_cran()
  skip_if_offline()
  v <- orphaversions()
  expect_true("2025" %in% v$version)
  withr::local_options(orphacodes.cache_dir = tempfile())
  db <- orphafetch("2025", include = character(), classifications = FALSE, use = FALSE)
  expect_s3_class(db, "orphadb")
  expect_equal(db$meta$version, "2025")
  expect_true(nrow(db$disorders) > 10000)
})

test_that("a non-English release parses to canonical labels", {
  skip_on_cran()
  skip_if_offline()
  withr::local_options(orphacodes.cache_dir = tempfile())
  fr <- orphafetch("2025", lang = "fr", include = character(), classifications = FALSE, use = FALSE)
  expect_true(all(fr$disorders$status %in% c("Active", "Active: Historic", "Inactive: Deprecated",
                                              "Inactive: Obsolete", "Inactive: Non rare disease in Europe", "Inactive")))
  expect_true(all(fr$disorders$level %in% c("Group of disorders", "Disorder", "Subtype of disorder")))
  expect_true(all(fr$redirects$relation %in% c("Moved to", "Referred to")))
  expect_true(nrow(fr$redirects) > 0)
  expect_true(any(grepl("Maladie", fr$disorders$name)))
})
