test_that("release diff detects changes beyond core entity fields", {
  old <- orphamini; new <- old
  new$xrefs$validation[1] <- "Changed validation"
  new$synonyms$synonym[1] <- "Changed synonym"
  new$disorders$definition[1] <- "Changed definition"
  new$edges$parent[1] <- "123456789"
  new$redirects$target[1] <- "123456789"
  new$prevalence$value[1] <- 123456
  d <- orphadiff(old, new)
  for (nm in c("xrefs", "synonyms", "disorders", "edges", "redirects", "prevalence")) {
    expect_gt(nrow(d$tables[[nm]]$added), 0L)
    expect_gt(nrow(d$tables[[nm]]$removed), 0L)
  }
  expect_equal(nrow(d$renamed), 0L)
  reordered <- old
  reordered$xrefs <- old$xrefs[rev(seq_len(nrow(old$xrefs))), ]
  expect_equal(nrow(orphadiff(old, reordered)$tables$xrefs$added), 0L)
  new <- old; new$genes <- NULL
  expect_equal(nrow(orphadiff(old, new)$tables$genes$removed), nrow(unique(old$genes)))
})

test_that("history spans supplied releases and retains absence", {
  old <- orphamini; old$meta$version <- "old"
  new <- old; new$meta$version <- "new"
  old$disorders <- old$disorders[old$disorders$orphacode != "324", ]
  h <- orphahistory("324", releases = list(old, new))
  expect_equal(h$release, c("old", "new"))
  expect_true(is.na(h$status[1]))
  expect_equal(h$status[2], "Active")
  expect_error(orphahistory("324", releases = list()), "nonempty")
})

# Synthetic chains deliberately exercise cases absent from the demo data.
chain_db <- function(n = 12L) {
  db <- orphamini
  rows <- as.data.frame(db$disorders[rep(1L, n), ])
  rows$orphacode <- as.character(seq_len(n) + 80000000L)
  rows$name <- paste("Chain", seq_len(n))
  rows$active <- FALSE; rows$status <- "Inactive: Deprecated"
  db$disorders <- rbind(db$disorders, rows)
  db$redirects <- data.frame(orphacode = rows$orphacode,
    target = c(rows$orphacode[-1], "324"), target_name = NA_character_, relation = "Moved to")
  db
}

test_that("replacement chains resolve fully and stop on ambiguity or cycles", {
  db <- chain_db()
  u <- orphaupdate("80000001", db = db)
  expect_equal(u$orphacode, "324")
  expect_equal(u$steps, 12L)
  expect_equal(u$update_issue, "ok")
  db$redirects$target[12] <- "80000001"
  expect_equal(orphaupdate("80000001", db = db)$update_issue, "replacement_cycle")
  db <- chain_db()
  branch <- db$redirects[1, ]; branch$target <- "558"
  db$redirects <- rbind(db$redirects, branch)
  u <- orphaupdate("80000001", db = db)
  expect_equal(u$update_issue, "ambiguous_replacement")
  expect_equal(u$orphacode, "80000001")
  expect_match(u$candidates, "558")
  expect_equal(nrow(orphaupdate(character(), db = db)), 0L)
  h <- orphaharmonize(data.frame(code = "80000001", dx = "Fabry disease"),
    code = "code", disease = "dx", db = db)
  expect_true(is.na(h$orphacode))
  expect_true(h$orphacode_review)
})

test_that("harmonisation rejects unresolved inactive codes and retains evidence", {
  db <- chain_db(); db$redirects <- db$redirects[FALSE, ]
  x <- data.frame(code = c("80000001", NA, "166068"),
                  dx = c(NA, "Fabry desease", NA))
  h <- orphaharmonize(x, code = "code", disease = "dx", db = db)
  expect_true(is.na(h$orphacode[1]))
  expect_equal(h$orphacode_input, x$code)
  expect_equal(h$orphacode_match[2], "fuzzy")
  expect_gt(h$orphacode_score[2], 0.9)
  expect_true(h$orphacode_review[2])
  h <- orphaharmonize(x, code = "code", db = db, active_only = FALSE)
  expect_equal(h$orphacode[1], "80000001")
  expect_true(h$orphacode_review[1])
  expect_equal(nrow(orphaharmonize(x[FALSE, ], code = "code", db = db)), 0L)
})

test_that("QC retains simultaneous findings and treats missing IDs separately", {
  x <- data.frame(id = c("a", "a", NA, ""), code = c("abc", "324", "324", "324"))
  q <- orphaqc(x, code = "code", id = "id", db = orphamini)
  expect_setequal(q$.issues[[1]], c("invalid_format", "duplicate_id"))
  expect_equal(q$.duplicate_id, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(q$.issues[[3]], "missing_id")
  expect_equal(q$.issues[[4]], "missing_id")
  expect_equal(attr(q, "summary_all")$n[attr(q, "summary_all")$issue == "duplicate_id"], 2L)
  expect_message(summary(q, all = TRUE), "quality control")
})

test_that("patient counting deduplicates within roll-up groups and strata", {
  x <- data.frame(id = c("a", "a", "a", "b"), code = rep("324", 4), year = c(2024,2024,2025,2025))
  expect_equal(sum(orphaaggregate(x, "code", db = orphamini)$n), 4L)
  expect_equal(sum(orphaaggregate(x, "code", db = orphamini, id = "id")$n), 2L)
  a <- orphaaggregate(x, "code", level = "specialty", by = "year", db = orphamini, id = "id")
  expect_equal(a$n[match(c(2024,2025), a$year)], c(1L,2L))
  a <- orphaaggregate(x, "code", level = "classification", db = orphamini, id = "id")
  expect_true(all(a$n == 2L))
  x$id[1] <- NA
  expect_error(orphaaggregate(x, "code", db = orphamini, id = "id"), "nonmissing")
})

xml_fixture <- function(date = "2025-07-01", extra = "") {
  path <- tempfile(fileext = ".xml")
  writeLines(paste0('<JDBOR date="', date, '"><DisorderList><Disorder>',
    '<OrphaCode>324</OrphaCode><Name>Fabry disease</Name><FlagValue>1</FlagValue>',
    '<ClassificationLevel id="36547"><Name>Disorder</Name></ClassificationLevel>',
    extra, '</Disorder></DisorderList></JDBOR>'), path)
  path
}

test_that("build records independent product dates and file fingerprints", {
  nom <- xml_fixture(); prev <- xml_fixture("2026-10-01")
  gene <- xml_fixture("2026-09-01")
  db <- orphabuild(nom, prevalence = prev, genes = gene, quiet = TRUE,
                  pack_commit = paste(rep("a", 40), collapse = ""))
  p <- db$meta$products
  expect_equal(p$extraction_date, as.Date(c("2025-07-01", "2026-10-01", "2026-09-01")))
  expect_equal(p$md5, unname(tools::md5sum(c(nom, prev, gene))))
  expect_true(all(is.na(p$pack_commit[-1])))
  expect_true(all(is.na(p$source)))
  f <- tempfile(); orphasave(db, f)
  expect_identical(orphaload(f, use = FALSE)$meta$products, p)
})

test_that("historical fetch defaults to pack-only and requires explicit mixing", {
  nom <- xml_fixture()
  withr::local_options(orphacodes.cache_dir = tempfile())
  calls <- character()
  testthat::local_mocked_bindings(
    resolveref = function(version) list(ref = paste(rep("a",40), collapse = ""), label = version),
    ghlist = function(ref, dir) if (dir == "ORPHAnomenclature") "ORPHAnomenclature_en.xml" else character(),
    getfile = function(url, dest, ...) {
      calls <<- c(calls, url)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(nom, dest, overwrite = TRUE)
      dest
    }, .package = "orphacodes")
  db <- orphafetch("2025", classifications = FALSE, use = FALSE, quiet = TRUE)
  expect_length(calls, 1L)
  expect_equal(db$meta$products$product, "nomenclature")
  expect_false(db$meta$supplementary_current)
  expect_error(orphafetch("2025", include = "prevalence"), "cannot silently")
  expect_warning(mixed <- orphafetch("2025", include = "prevalence", allow_current = TRUE,
    classifications = FALSE, use = FALSE, quiet = TRUE), "not pinned")
  expect_true(mixed$meta$supplementary_current)
  expect_true("prevalence" %in% mixed$meta$products$product)
})

test_that("mapping ingestion retains differing relation and validation records", {
  ref <- function(relation) paste0('<ExternalReference><Source>ICD10</Source><Reference>E75.2</Reference>',
    '<DisorderMappingRelation id="', relation, '"><Name>relation</Name></DisorderMappingRelation></ExternalReference>')
  path <- xml_fixture(extra = paste0('<ExternalReferenceList>', ref("21527"), ref("21534"), '</ExternalReferenceList>'))
  db <- orphabuild(path, icd10 = path, quiet = TRUE)
  expect_equal(nrow(db$xrefs), 2L)
  expect_setequal(db$xrefs$relation, c("E", "NTBT"))
})

test_that("empty registry extracts retain usable result schemas", {
  x <- data.frame(id = character(), code = character(), year = integer())
  q <- orphaqc(x, "code", id = "id", db = orphamini)
  expect_equal(nrow(q), 0L)
  expect_equal(nrow(attr(q, "summary_all")), 0L)
  for (level in c("code", "disorder", "specialty", "classification")) {
    a <- orphaaggregate(x, "code", level = level, id = "id", by = "year", db = orphamini)
    expect_equal(nrow(a), 0L)
    expect_named(a, c("group", "group_name", "year", "n"))
  }
})

test_that("invalid inputs and repeated codes retain distinct audit results", {
  u <- orphaupdate(c("abc", NA, "", "324", "324"), db = orphamini)
  expect_equal(u$update_issue, c("invalid_format", "missing", "missing", "ok", "ok"))
  expect_equal(u$orphacode[4:5], c("324", "324"))
})

test_that("latest pack is resolved to an immutable commit before downloads", {
  sha <- paste(rep("b", 40), collapse = "")
  testthat::local_mocked_bindings(ghapi = function(...) list(sha = sha), .package = "orphacodes")
  expect_equal(resolveref("latest")$ref, sha)
})
