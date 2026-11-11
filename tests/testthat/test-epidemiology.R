h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis", db = orphamini)

test_that("orphaaggregate counts at each level", {
  a <- orphaaggregate(h, code = "orphacode", level = "code", db = orphamini)
  expect_equal(sum(a$n), nrow(h))
  b <- orphaaggregate(h, code = "orphacode", level = "specialty", by = "sex", db = orphamini)
  expect_equal(sum(b$n), nrow(h))
  expect_true("sex" %in% names(b))
})

test_that("orphaprevref picks one class per code", {
  r <- orphaprevref(c("324", "558"), db = orphamini, best = TRUE)
  expect_equal(r$orphacode, c("324", "558"))
  expect_true(nrow(orphaprevref("324", db = orphamini, best = FALSE)) >= 1)
})
