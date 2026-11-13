#' Choice lists of ORPHAcodes for data capture
#'
#' Builds a coded value list (code and label) from the nomenclature for use
#' in a data-collection form, a database constraint or a validation rule.
#' `orpharedcap()` formats the same list as a REDCap data dictionary row
#' (an autocomplete dropdown) that can be imported into any REDCap project.
#'
#' @param db An [orphadb]; defaults to the registered database.
#' @param level Classification levels to include; defaults to
#'   `"Disorder"` and `"Subtype of disorder"` (group codes are not meant for
#'   coding patients).
#' @param codes Optional explicit set of ORPHAcodes, for example the
#'   descendants of one classification obtained with [orphadescendants()].
#' @param active_only Logical; drop inactive codes.
#' @param synonyms Logical; append synonyms to the label (between
#'   parentheses) so that users searching a synonym find the code.
#' @param prefix Logical; label with the `"ORPHA:"` prefix.
#'
#' @return `orphachoices()`: a data frame with `code`, `label`, `name`,
#'   `level`. `orpharedcap()`: a data frame with the REDCap data dictionary
#'   columns (one row), written to `file` if given.
#' @export
#' @examples
#' head(orphachoices(db = orphamini))
#' dd <- orpharedcap(db = orphamini, field = "rare_disease")
#' names(dd)
orphachoices <- function(db = NULL, level = c("Disorder", "Subtype of disorder"),
                         codes = NULL, active_only = TRUE, synonyms = FALSE,
                         prefix = FALSE) {
  db <- getdb(db)
  d <- db$disorders
  if (!is.null(codes)) d <- d[d$orphacode %in% orphacode(codes), , drop = FALSE]
  if (!is.null(level)) d <- d[d$level %in% level, , drop = FALSE]
  if (isTRUE(active_only)) d <- d[d$active, , drop = FALSE]
  label <- d$name
  if (isTRUE(synonyms)) {
    s <- basetable::aggregate(db$synonyms[db$synonyms$orphacode %in% d$orphacode, ],
                              by = "orphacode", value = "synonym",
                              fun = function(v) paste(v, collapse = "; "))
    extra <- pick(d$orphacode, s, "orphacode", "synonym")
    label <- ifelse(is.na(extra), label, paste0(label, " (", extra, ")"))
  }
  out <- data.frame(code = orphacode(d$orphacode, prefix = prefix), label = label,
                    name = d$name, level = d$level, stringsAsFactors = FALSE)
  ascdf(basetable::orderrows(out, "name"))
}

#' @rdname orphachoices
#' @param field Variable name of the REDCap field.
#' @param form Form (instrument) name.
#' @param label_field Field label shown to the user.
#' @param file Optional path; the dictionary is written as a CSV ready for
#'   REDCap import.
#' @param ... Passed to `orphachoices()`.
#' @export
orpharedcap <- function(db = NULL, field = "orphacode", form = "diagnosis",
                        label_field = "Rare disease (Orphanet)", file = NULL, ...) {
  ch <- orphachoices(db = db, ...)
  choices <- paste(ch$code, gsub("|", "/", ch$label, fixed = TRUE), sep = ", ", collapse = " | ")
  dd <- data.frame(
    `Variable / Field Name` = field,
    `Form Name` = form,
    `Section Header` = "",
    `Field Type` = "dropdown",
    `Field Label` = label_field,
    `Choices, Calculations, OR Slider Labels` = choices,
    `Field Note` = paste0("ORPHAcode, Orphanet nomenclature ", getdb(db)$meta$version %||% ""),
    `Text Validation Type OR Show Slider Number` = "autocomplete",
    `Text Validation Min` = "", `Text Validation Max` = "", `Identifier?` = "",
    `Branching Logic (Show field only if...)` = "", `Required Field?` = "",
    `Custom Alignment` = "", `Question Number (surveys only)` = "",
    `Matrix Group Name` = "", `Matrix Ranking?` = "", `Field Annotation` = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!is.null(file)) {
    basetable::btwrite(dd, file, na = "")
    cli::cli_alert_success("REDCap data dictionary written to {.path {file}} ({nrow(ch)} choices).")
  }
  ascdf(dd)
}

#' Quality control of ORPHAcodes in a registry
#'
#' Audits a registry data frame: code format, presence in the release,
#' status (deprecated, obsolete, non-rare), classification level (group
#' codes used for patients), and duplicated identifiers if an `id` column
#' is given. Returns the row-level findings; `summary()` gives the counts.
#'
#' @param data A data frame.
#' @param code Name of the column holding ORPHAcodes.
#' @param id Optional name of the patient identifier column (duplicates
#'   are reported).
#' @param db An [orphadb]; defaults to the registered database.
#' @param allow_groups Logical; if `FALSE` (default) group-level codes are
#'   flagged as `"group_code"`.
#'
#' @return An object of class `orphaqc`: `data` with the columns
#'   `.orphacode` (normalised), `.issue`, `.name`, `.replacement` added,
#'   plus `.issues` (a list column of all findings), `.duplicate_id` and
#'   an attribute `summary`. `.issue` retains the primary finding for
#'   compatibility. `summary_all` counts every finding, so percentages
#'   can total more than 100. Missing/blank identifiers are flagged as
#'   `missing_id`, never treated as duplicate patients.
#' @export
#' @examples
#' qc <- orphaqc(orphademo, code = "diagnosis_code", id = "patient_id", db = orphamini)
#' summary(qc)
orphaqc <- function(data, code, id = NULL, db = NULL, allow_groups = FALSE) {
  assertdf(data)
  assertcol(data, code, "code")
  db <- getdb(db)
  v <- orphavalidate(data[[code]], db = db)
  lvl <- pick(v$orphacode, db$disorders, "orphacode", "level")
  issue <- v$issue
  if (!isTRUE(allow_groups)) issue[issue == "ok" & lvl %in% "Group of disorders"] <- "group_code"
  issues <- lapply(issue, function(x) if (x == "ok") character() else x)
  dup <- rep(FALSE, nrow(data))
  if (!is.null(id)) {
    assertcol(data, id, "id")
    missing_id <- is.na(data[[id]]) | !nzchar(trimws(as.character(data[[id]])))
    dup <- !missing_id & (duplicated(data[[id]]) | duplicated(data[[id]], fromLast = TRUE))
    for (i in which(dup)) issues[[i]] <- c(issues[[i]], "duplicate_id")
    for (i in which(missing_id)) issues[[i]] <- c(issues[[i]], "missing_id")
    issue[dup & issue == "ok"] <- "duplicate_id"
    issue[missing_id & issue == "ok"] <- "missing_id"
  }
  # Classification findings are independent of code status.
  group <- !isTRUE(allow_groups) & lvl %in% "Group of disorders"
  for (i in which(group)) issues[[i]] <- unique(c(issues[[i]], "group_code"))
  issues <- lapply(issues, function(x) if (length(x)) x else "ok")
  data$.issues <- I(issues)
  data$.duplicate_id <- dup
  data$.orphacode <- v$orphacode
  data$.issue <- issue
  data$.name <- v$name
  data$.replacement <- v$replacement
  out <- ascdf(data)
  s <- basetable::count(data.frame(issue = issue), "issue")
  s$pct <- round(100 * s$n / nrow(data), 1)
  attr(out, "summary") <- ascdf(s)
  all <- basetable::count(data.frame(issue = as.character(unlist(issues, use.names = FALSE))), "issue")
  all$pct <- round(100 * all$n / nrow(data), 1)
  attr(out, "summary_all") <- ascdf(all)
  attr(out, "version") <- db$meta$version
  class(out) <- c("orphaqc", class(out))
  out
}

#' @rdname orphaqc
#' @param object An `orphaqc` result.
#' @param all Logical; summarise all findings instead of primary findings.
#' @param ... Reserved for additional summary arguments.
#' @export
summary.orphaqc <- function(object, ..., all = FALSE) {
  s <- attr(object, if (all) "summary_all" else "summary")
  cli::cli_h2("ORPHAcode quality control ({nrow(object)} rows, Orphanet {attr(object, 'version')})")
  print(s)
  invisible(s)
}

#' @export
print.orphaqc <- function(x, ...) {
  y <- x
  class(y) <- setdiff(class(y), "orphaqc")
  attr(y, "summary") <- NULL
  attr(y, "version") <- NULL
  print(y, ...)
  invisible(x)
}

#' Harmonise registry codes to one release
#'
#' One-stop harmonisation of a registry: codes are normalised, deprecated
#' codes replaced through `"Moved to"`, free-text names standardised when
#' no code is given, and the aggregation-level and preferential-parent
#' columns added so that the table can be aggregated and compared with
#' other registries coded on the same release.
#'
#' @param data A data frame.
#' @param code Name of the ORPHAcode column (may contain `NA`).
#' @param disease Optional name of a disease-name column used when the
#'   code is missing or invalid.
#' @param db An [orphadb]; defaults to the registered database.
#' @param active_only Logical; refuse unresolved inactive codes by default.
#'   Set `FALSE` to retain them with an explicit review flag.
#' @param follow Replacement policy passed to [orphaupdate()].
#' @param ... Passed to [orphastandardize()] (for example `fuzzy`,
#'   `max_distance`).
#'
#' @return `data` with columns `orphacode` (harmonised), `orphacode_name`,
#'   `orphacode_source` (`"code"`, `"updated"`, `"name"` or `"none"`),
#'   `orphacode_level`, `aggregation_code`, `aggregation_name`,
#'   `preferential_parent`, `preferential_parent_name`. Audit columns retain
#'   the original code, replacement path and issue, matching tier and score,
#'   output status, and `orphacode_review`. Fuzzy matches, inactive outputs
#'   and unresolved inputs require review. Ambiguous replacements and cycles
#'   are never overridden by name matching.
#' @export
#' @examples
#' h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis", db = orphamini)
#' basetable::count(h, "orphacode_source")
orphaharmonize <- function(data, code, disease = NULL, db = NULL,
                           follow = c("moved", "all"), active_only = TRUE, ...) {
  assertdf(data)
  assertcol(data, code, "code")
  db <- getdb(db)
  original <- as.character(data[[code]])
  up <- orphaupdate(data[[code]], db = db, follow = follow)
  blocked <- up$update_issue %in% c("ambiguous_replacement", "replacement_cycle")
  active <- db$disorders$active[match(up$orphacode, db$disorders$orphacode)] %in% TRUE
  valid <- !is.na(up$orphacode) & !is.na(up$name) & !blocked & (!active_only | active)
  match_method <- rep(NA_character_, nrow(data))
  match_score <- rep(NA_real_, nrow(data))
  out <- up$orphacode
  out[!valid] <- NA_character_
  src <- ifelse(valid & up$changed, "updated", ifelse(valid, "code", "none"))
  if (!is.null(disease)) {
    assertcol(data, disease, "disease")
    todo <- !valid & !blocked & !is.na(data[[disease]])
    if (any(todo)) {
      st <- orphastandardize(data[todo, disease, drop = FALSE], disease = disease, db = db, active_only = active_only, ...)
      match_method[todo] <- st$orphacode_match
      match_score[todo] <- st$orphacode_score
      ok <- !is.na(st$orphacode)
      out[which(todo)[ok]] <- st$orphacode[ok]
      src[which(todo)[ok]] <- "name"
    }
  }
  d <- db$disorders
  i <- match(out, d$orphacode)
  data$orphacode_input <- original
  data$orphacode_update_issue <- up$update_issue
  data$orphacode_path <- up$path
  data$orphacode_candidates <- up$candidates
  data$orphacode_match <- match_method
  data$orphacode_score <- match_score
  data$orphacode_status <- d$status[i]
  data$orphacode_review <- is.na(out) | !(d$active[i] %in% TRUE) |
    match_method %in% c("fuzzy", "ambiguous") | grepl("Referred to", up$relation, fixed = TRUE)
  data$orphacode <- out
  data$orphacode_name <- d$name[i]
  data$orphacode_source <- src
  data$orphacode_level <- d$level[i]
  data$aggregation_code <- ifelse(is.na(d$aggregation_code[i]) & d$level[i] %in% "Disorder", out, d$aggregation_code[i])
  data$aggregation_name <- d$name[match(data$aggregation_code, d$orphacode)]
  data$preferential_parent <- d$preferential_parent[i]
  data$preferential_parent_name <- d$preferential_parent_name[i]
  ascdf(data)
}
