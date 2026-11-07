h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis", db = orphamini)

test_that("orphaaggregate counts at each level", {
  a <- orphaaggregate(h, code = "orphacode", level = "code", db = orphamini)
  expect_equal(sum(a$n), nrow(h))
  b <- orphaaggregate(h, code = "orphacode", level = "specialty", by = "sex", db = orphamini)
  expect_equal(sum(b$n), nrow(h))
  expect_true("sex" %in% names(b))
})

test_that("orphaprevalence gives Poisson intervals and reference classes", {
  p <- orphaprevalence(h, code = "orphacode", population = 5e6, level = "disorder", db = orphamini)
  expect_true(all(p$lower <= p$per100k & p$per100k <= p$upper))
  expect_true(any(!is.na(p$orphanet_class)))
  expect_error(orphaprevalence(h, code = "orphacode", population = -1, db = orphamini))
})

test_that("orphaprevref picks one class per code", {
  r <- orphaprevref(c("324", "558"), db = orphamini, best = TRUE)
  expect_equal(r$orphacode, c("324", "558"))
  expect_true(nrow(orphaprevref("324", db = orphamini, best = FALSE)) >= 1)
})

test_that("stratified and standardised prevalence are consistent", {
  pop <- data.frame(sex = c("F", "M"), population = c(2.6e6, 2.4e6))
  s <- orphaprevalence(h, code = "orphacode", population = pop, by = "sex",
                       level = "specialty", db = orphamini)
  expect_true(all(c("sex", "n", "population", "per100k") %in% names(s)))
  expect_true(all(s$lower <= s$per100k & s$per100k <= s$upper))
  tot <- basetable::aggregate(s, by = "group", value = "n", fun = sum)
  crude <- orphaprevalence(h, code = "orphacode", population = 5e6, level = "specialty", db = orphamini)
  expect_equal(sum(tot$n), sum(crude$n))
  std <- data.frame(sex = c("F", "M"), weight = c(0.5, 0.5))
  z <- orphaprevalence(h, code = "orphacode", population = pop, by = "sex", standard = std,
                       level = "specialty", db = orphamini)
  expect_false("sex" %in% names(z))
  expect_true(all(z$lower <= z$per100k & z$per100k <= z$upper))
  expect_true(all(z$method == "standardised"))
  # Fay-Feuer with a single stratum of weight 1 reduces to the crude Poisson interval
  h$all <- "all"
  z1 <- orphaprevalence(h, code = "orphacode", population = data.frame(all = "all", population = 5e6),
                        by = "all", standard = data.frame(all = "all", weight = 1),
                        level = "specialty", db = orphamini)
  m <- match(z1$group, crude$group)
  expect_equal(z1$per100k, crude$per100k[m])
  expect_equal(z1$lower, crude$lower[m], tolerance = 1e-6)
})
