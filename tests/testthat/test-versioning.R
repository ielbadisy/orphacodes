test_that("orphaupdate follows Moved to", {
  u <- orphaupdate(c("166068", "324", "abc"), db = orphamini)
  expect_equal(u$orphacode, c("166063", "324", NA))
  expect_equal(u$changed, c(TRUE, FALSE, FALSE))
  expect_equal(u$steps, c(1L, 0L, 0L))
  h <- orphahistory("166068", db = orphamini)
  expect_equal(h$target, "166063")
})

test_that("orphadiff reports changes", {
  d2 <- orphamini
  d2$disorders$name[1] <- "Renamed"
  k <- which(d2$disorders$active)[5]
  d2$disorders$status[k] <- "Inactive: Deprecated"
  d2$disorders <- d2$disorders[-3, ]
  d2$meta$version <- "next"
  d <- orphadiff(orphamini, d2)
  expect_s3_class(d, "orphadiff")
  expect_equal(nrow(d$renamed), 1L)
  expect_equal(nrow(d$status), 1L)
  expect_equal(nrow(d$removed), 1L)
  expect_equal(nrow(d$added), 0L)
  expect_message(print(d), "renamed")
})

test_that("orphasave and orphaload round trip", {
  f <- tempfile(fileext = ".rds")
  orphasave(orphamini, f)
  db <- orphaload(f, use = FALSE)
  expect_equal(nrow(db$disorders), nrow(orphamini$disorders))
})

test_that("orphadiff renamed table has old and new names", {
  d2 <- orphamini
  i <- which(d2$disorders$active)[3]
  d2$disorders$name[i] <- paste(d2$disorders$name[i], "(renamed)")
  d <- orphadiff(orphamini, d2)
  expect_equal(names(d$renamed), c("orphacode", "name_new", "name_old"))
  expect_equal(nrow(d$renamed), 1L)
})
