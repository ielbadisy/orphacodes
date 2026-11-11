#' Count registry cases by ORPHAcode or by roll-up level
#'
#' Counts rows per code after rolling each code up to the requested level:
#' the code itself, the EU aggregation level (subtypes counted under their
#' disorder), the preferential parent (head of the linearised
#' classification, no double counting), or every classification an entity
#' belongs to (a case counted once per classification, so totals exceed
#' the number of rows).
#'
#' @param data A data frame.
#' @param code Name of the ORPHAcode column.
#' @param level `"code"`, `"disorder"`, `"specialty"` or
#'   `"classification"` (see [orphagroup()]).
#' @param by Optional additional grouping columns (for example `"sex"`,
#'   `"year"`).
#' @param db An [orphadb]; defaults to the registered database.
#'
#' @return A data frame with `group`, `group_name`, the `by` columns and
#'   `n`, sorted by decreasing `n`. Rows whose code cannot be rolled up are
#'   reported under `group = NA`.
#' @export
#' @examples
#' h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis", db = orphamini)
#' orphaaggregate(h, code = "orphacode", level = "disorder", db = orphamini)
#' orphaaggregate(h, code = "orphacode", level = "specialty", by = "sex", db = orphamini)
orphaaggregate <- function(data, code, level = c("code", "disorder", "specialty", "classification"),
                           by = NULL, db = NULL) {
  assertdf(data)
  assertcol(data, code, "code")
  db <- getdb(db)
  level <- match.arg(level)
  x <- orphacode(data[[code]])
  if (level == "code") {
    g <- data.frame(orphacode = x, group = x, stringsAsFactors = FALSE)
    g$group_name <- pick(x, db$disorders, "orphacode", "name")
    tab <- cbind(g[c("group", "group_name")], data[by])
  } else {
    gr <- orphagroup(unique(x[!is.na(x)]), db = db, level = level)
    key <- data.frame(orphacode = x, .row = seq_along(x), stringsAsFactors = FALSE)
    m <- basetable::merge(key, gr[c("orphacode", "group", "group_name")], by = "orphacode", all.x = TRUE)
    m <- basetable::orderrows(m, ".row")
    tab <- cbind(m[c("group", "group_name")], data[m$.row, by, drop = FALSE])
  }
  out <- basetable::count(tab, c("group", "group_name", by), sort = TRUE)
  ascdf(out)
}

#' Orphanet reference prevalence
#'
#' Returns the epidemiological data Orphanet publishes for each code
#' (point prevalence, birth prevalence, lifetime prevalence, annual
#' incidence, number of cases/families) with their geographic scope and
#' validation status. With `best = TRUE`, one point-prevalence class per
#' code is chosen according to `geographic`.
#'
#' @param codes ORPHAcodes.
#' @param db An [orphadb]; defaults to the registered database.
#' @param type Optional subset of prevalence types, for example
#'   `"Point prevalence"`.
#' @param geographic Preferred geographic scopes, in order.
#' @param best Logical; one row per code.
#'
#' @return A data frame with `orphacode`, `name`, `type`, `qualification`,
#'   `class`, `value`, `geographic`, `validation`, `source`.
#' @export
#' @examples
#' orphaprevref("324", db = orphamini)
#' orphaprevref(c("324", "558"), db = orphamini, best = TRUE)
orphaprevref <- function(codes, db = NULL, type = NULL,
                         geographic = c("Europe", "Worldwide"), best = TRUE) {
  db <- getdb(db)
  code <- orphacode(codes)
  p <- db$prevalence[db$prevalence$orphacode %in% code, , drop = FALSE]
  if (!is.null(type)) p <- p[p$type %in% type, , drop = FALSE]
  if (isTRUE(best)) {
    p <- p[p$type %in% "Point prevalence" & !p$class %in% c("Unknown", "Not yet documented", NA), , drop = FALSE]
    p$rank <- match(p$geographic, c(geographic, "Worldwide"))
    p$rank[is.na(p$rank)] <- length(geographic) + 2L
    p$rank <- p$rank * 2 + (!p$validation %in% "Validated")
    p <- basetable::removeduplicates(basetable::orderrows(p, c("orphacode", "rank")), by = "orphacode")
    p$rank <- NULL
  }
  p$name <- pick(p$orphacode, db$disorders, "orphacode", "name")
  p <- p[order(match(p$orphacode, code)), , drop = FALSE]
  ascdf(p[c("orphacode", "name", "type", "qualification", "class", "value", "geographic", "validation", "source")])
}
