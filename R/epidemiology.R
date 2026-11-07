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

#' Observed prevalence in a registry beside the Orphanet reference class
#'
#' Computes, per code (or per roll-up level), the number of cases, the
#' prevalence per 100,000 with a confidence interval, and places it next to
#' the point prevalence class published by Orphanet for the same entity.
#' Three designs are supported:
#'
#' * crude: `population` is one number, the catchment population, and the
#'   interval is the exact Poisson interval;
#' * stratified: `by` names strata columns (for example age group and sex)
#'   and `population` is a data frame with the same columns plus a
#'   `population` column; one row per entity and stratum is returned;
#' * directly standardised: in addition, `standard` is a data frame with
#'   the strata columns and a `weight` column (a standard population); the
#'   stratum rates are combined with those weights and the interval is the
#'   gamma interval of Fay and Feuer (1997), which is valid for the small
#'   counts typical of rare diseases.
#'
#' The function does not model under-ascertainment: an observed rate is a
#' lower bound of the true prevalence whenever the registry does not
#' capture every case, and `population` must be the population the
#' registry actually covers. Strata with cases but no population row are
#' dropped with a warning.
#'
#' @param data A data frame.
#' @param code Name of the ORPHAcode column.
#' @param population Catchment population: a single number, or a data
#'   frame with the `by` columns and a `population` column.
#' @param level Roll-up level, as in [orphaaggregate()].
#' @param by Optional strata columns.
#' @param standard Optional data frame with the `by` columns and a
#'   `weight` column; triggers direct standardisation.
#' @param db An [orphadb]; defaults to the registered database.
#' @param conf_level Confidence level.
#' @param geographic Geographic scope used to pick the Orphanet reference
#'   (`"Europe"`, `"Worldwide"` or a country name), tried in that order
#'   with `"Worldwide"` as fallback.
#'
#' @return A data frame with `group`, `group_name`, the strata columns if
#'   any, `n`, `population`, `per100k`, `lower`, `upper`, `method`
#'   (`"crude"`, `"stratified"` or `"standardised"`), `orphanet_class`,
#'   `orphanet_source`.
#' @references Fay MP, Feuer EJ (1997). Confidence intervals for directly
#'   standardized rates: a method based on the gamma distribution.
#'   Statistics in Medicine 16(7), 791-801.
#' @export
#' @examples
#' h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis", db = orphamini)
#' orphaprevalence(h, code = "orphacode", population = 5e6, level = "disorder", db = orphamini)
#' pop <- data.frame(sex = c("F", "M"), population = c(2.6e6, 2.4e6))
#' std <- data.frame(sex = c("F", "M"), weight = c(0.5, 0.5))
#' orphaprevalence(h, code = "orphacode", population = pop, by = "sex",
#'                 level = "specialty", db = orphamini)
#' orphaprevalence(h, code = "orphacode", population = pop, by = "sex",
#'                 standard = std, level = "specialty", db = orphamini)
orphaprevalence <- function(data, code, population, level = "disorder", by = NULL,
                            standard = NULL, db = NULL, conf_level = 0.95,
                            geographic = "Europe") {
  db <- getdb(db)
  a <- orphaaggregate(data, code = code, level = level, by = by, db = db)
  a <- a[!is.na(a$group), , drop = FALSE]
  alpha <- 1 - conf_level
  if (is.null(by)) {
    if (!is.numeric(population) || length(population) != 1L || population <= 0) {
      cli::cli_abort("{.arg population} must be a single positive number when {.arg by} is NULL.")
    }
    a$population <- population
    ci <- poisci(a$n, alpha)
    a$per100k <- 1e5 * a$n / population
    a$lower <- 1e5 * ci[, 1] / population
    a$upper <- 1e5 * ci[, 2] / population
    a$method <- "crude"
  } else {
    if (!is.data.frame(population) || !all(c(by, "population") %in% names(population))) {
      cli::cli_abort("{.arg population} must be a data frame with columns {.val {c(by, 'population')}}.")
    }
    full <- basetable::completegrid(a, c("group", by))
    full <- full[c("group", by)]
    full <- basetable::merge(full, a, by = c("group", by), all.x = TRUE)
    full$n[is.na(full$n)] <- 0L
    full$group_name <- pick(full$group, a, "group", "group_name")
    full <- basetable::merge(full, population[c(by, "population")], by = by, all.x = TRUE)
    miss <- is.na(full$population)
    if (any(miss)) {
      cli::cli_warn("{sum(full$n[miss])} case{?s} in {sum(miss)} stratum/strata without a population row dropped.")
      full <- full[!miss, , drop = FALSE]
    }
    if (is.null(standard)) {
      ci <- poisci(full$n, alpha)
      full$per100k <- 1e5 * full$n / full$population
      full$lower <- 1e5 * ci[, 1] / full$population
      full$upper <- 1e5 * ci[, 2] / full$population
      full$method <- "stratified"
      a <- full
    } else {
      if (!is.data.frame(standard) || !all(c(by, "weight") %in% names(standard))) {
        cli::cli_abort("{.arg standard} must be a data frame with columns {.val {c(by, 'weight')}}.")
      }
      standard$weight <- standard$weight / sum(standard$weight)
      full <- basetable::merge(full, standard[c(by, "weight")], by = by, all.x = TRUE)
      if (anyNA(full$weight)) cli::cli_abort("Every stratum needs a row in {.arg standard}.")
      full$w <- full$weight / full$population
      agg <- basetable::applyby(full, "group", function(g) {
        r <- fayfeuer(g$n, g$w, alpha)
        data.frame(group = g$group[1], group_name = g$group_name[1], n = sum(g$n),
                   population = sum(g$population), per100k = 1e5 * r[1],
                   lower = 1e5 * r[2], upper = 1e5 * r[3], stringsAsFactors = FALSE)
      }, bind = TRUE)
      agg$.group <- NULL
      agg$method <- "standardised"
      a <- agg
    }
  }
  a <- basetable::orderrows(a, "n", decreasing = TRUE)
  for (v in c("per100k", "lower", "upper")) a[[v]] <- round(a[[v]], 3)
  ref <- orphaprevref(unique(a$group), db = db, geographic = geographic)
  a$orphanet_class <- ref$class[match(a$group, ref$orphacode)]
  a$orphanet_source <- ref$geographic[match(a$group, ref$orphacode)]
  strata <- if (is.null(standard)) by
  ascdf(a[c("group", "group_name", strata, "n", "population", "per100k", "lower", "upper",
            "method", "orphanet_class", "orphanet_source")])
}

# Exact Poisson interval for counts (vectorised).
poisci <- function(n, alpha) {
  lo <- ifelse(n == 0, 0, stats::qchisq(alpha / 2, 2 * n) / 2)
  hi <- stats::qchisq(1 - alpha / 2, 2 * (n + 1)) / 2
  cbind(lo, hi)
}

# Fay and Feuer (1997) gamma interval for a directly standardised rate
# sum(w_i * n_i), with w_i = weight_i / population_i.
fayfeuer <- function(n, w, alpha) {
  rate <- sum(w * n)
  v <- sum(w^2 * n)
  wm <- max(w)
  lo <- if (rate == 0) 0 else stats::qgamma(alpha / 2, shape = rate^2 / v, scale = v / rate)
  hi <- stats::qgamma(1 - alpha / 2, shape = (rate + wm)^2 / (v + wm^2), scale = (v + wm^2) / (rate + wm))
  c(rate, lo, hi)
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
