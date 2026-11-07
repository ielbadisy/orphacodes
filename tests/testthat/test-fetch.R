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
