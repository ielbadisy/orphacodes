test_that("orphamap keeps relation types and best picks one", {
  m <- orphamap("324", to = "ICD10", db = orphamini)
  expect_equal(m$code, "E75.2")
  expect_equal(m$relation, "NTBT")
  b <- orphamap(c("324", "558"), to = "OMIM", db = orphamini, best = TRUE)
  expect_equal(nrow(b), 2L)
  expect_equal(b$relation[1], "E")
  expect_equal(orphamap("1", to = "ICD10", db = orphamini)$code, NA_character_)
})

test_that("orphafrom reverses the mapping", {
  f <- orphafrom("E75.2", from = "icd-10", db = orphamini)
  expect_true("324" %in% f$orphacode)
  expect_true(all(f$source == "ICD10"))
})

test_that("orphasources summarises", {
  s <- orphasources(orphamini)
  expect_true(all(c("ICD10", "OMIM") %in% s$source))
  expect_true(all(s$n_mappings >= s$n_orphacodes))
})
