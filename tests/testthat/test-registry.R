test_that("orphaqc flags the simulated defects", {
  qc <- orphaqc(orphademo, code = "diagnosis_code", id = "patient_id", db = orphamini)
  s <- attr(qc, "summary")
  expect_true(all(c("ok", "missing", "deprecated", "group_code", "invalid_format", "duplicate_id") %in% s$issue))
  expect_equal(sum(s$n), nrow(orphademo))
  expect_message(summary(qc), "quality control")
})

test_that("orphaharmonize recovers codes from names and replacements", {
  h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis", db = orphamini)
  expect_true(all(c("code", "updated", "name") %in% h$orphacode_source))
  expect_true(all(h$orphacode[h$orphacode_source != "none"] %in% orphamini$disorders$orphacode))
  expect_true(all(is.na(h$orphacode[h$orphacode_source == "none"])))
})

test_that("orphachoices and orpharedcap produce a dictionary", {
  ch <- orphachoices(db = orphamini)
  expect_true(all(ch$level %in% c("Disorder", "Subtype of disorder")))
  dd <- orpharedcap(db = orphamini, field = "rd")
  expect_equal(dd[["Variable / Field Name"]], "rd")
  expect_equal(dd[["Text Validation Type OR Show Slider Number"]], "autocomplete")
  f <- tempfile(fileext = ".csv")
  expect_message(orpharedcap(db = orphamini, file = f), "written")
  expect_true(file.exists(f))
})
