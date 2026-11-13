#' History and replacement of ORPHAcodes
#'
#' `orphahistory()` reports, for each code, its status in the release and
#' the associations Orphanet records with other codes: `"Moved to"` for
#' deprecated entities (the replacement is mandatory) and `"Referred to"`
#' for obsolete ones (a suggestion, to be reviewed). `orphaupdate()` applies
#' the `"Moved to"` replacements to a vector of codes, following chains
#' until an active code is reached, and optionally the `"Referred to"`
#' suggestions.
#'
#' @param codes ORPHAcodes.
#' @param db An [orphadb]; defaults to the registered database.
#' @param follow Which associations `orphaupdate()` applies: `"moved"`
#'   (deprecated codes only, the safe default) or `"all"` (also obsolete
#'   codes through their "Referred to" suggestion).
#'
#' @param releases Optional list of databases, in chronological order. When
#'   supplied, history is returned for every release with `release` and
#'   `extraction_date` columns, including codes absent from a release.
#' @return `orphahistory()`: a data frame with `orphacode`, `name`,
#'   `status`, `relation`, `target`, `target_name`, `target_status`.
#'   `orphaupdate()`: a data frame with `input`, `orphacode` (the updated
#'   code, or the input when no replacement applies), `name`, `changed`,
#'   `steps`, `relation`, `status`, `update_issue`, `path`, `candidates`.
#'   Branches and cycles stop at the last unambiguous code and are flagged;
#'   chains have no arbitrary length limit. Only `update_issue == "ok"`
#'   establishes that an active target was reached.
#' @export
#' @examples
#' orphahistory(c("166068", "324"), db = orphamini)
#' orphaupdate(c("166068", "324"), db = orphamini)
orphahistory <- function(codes, db = NULL, releases = NULL) {
  if (!is.null(releases)) {
    if (!length(releases) || !all(vapply(releases, isdb, logical(1))))
      cli::cli_abort("{.arg releases} must be a nonempty list of databases in chronological order.")
    return(rowbind(lapply(releases, function(release) {
      h <- orphahistory(codes, db = release)
      h$release <- rep(release$meta$version %||% NA_character_, nrow(h))
      h$extraction_date <- rep(release$meta$extraction_date %||% as.Date(NA), nrow(h))
      h
    })))
  }
  db <- getdb(db)
  code <- orphacode(codes)
  d <- db$disorders
  q <- data.frame(orphacode = unique(code[!is.na(code)]), stringsAsFactors = FALSE)
  out <- basetable::merge(q, db$redirects, by = "orphacode", all.x = TRUE)
  out$name <- pick(out$orphacode, d, "orphacode", "name")
  out$status <- pick(out$orphacode, d, "orphacode", "status")
  out$target_status <- pick(out$target, d, "orphacode", "status")
  out <- out[order(match(out$orphacode, code)), , drop = FALSE]
  ascdf(out[c("orphacode", "name", "status", "relation", "target", "target_name", "target_status")])
}

#' @rdname orphahistory
#' @export
orphaupdate <- function(codes, db = NULL, follow = c("moved", "all")) {
  db <- getdb(db)
  follow <- match.arg(follow)
  code <- orphacode(codes)
  r <- db$redirects
  if (follow == "moved") r <- r[r$relation == "Moved to", , drop = FALSE]
  d <- db$disorders
  # Resolve each chain independently; never select an arbitrary branch.
  keys <- unique(code)
  resolved <- lapply(keys, function(start) {
    cur <- start; path <- start; steps <- 0L; relations <- character()
    candidates <- character()
    issue <- if (is.na(start)) "invalid_format" else "unknown"
    while (!is.na(cur)) {
      i <- match(cur, d$orphacode)
      if (!is.na(i) && isTRUE(d$active[i])) { issue <- "ok"; break }
      targets <- unique(r$target[r$orphacode %in% cur])
      targets <- targets[!is.na(targets) & nzchar(targets)]
      if (!length(targets)) {
        issue <- if (is.na(i)) "unknown" else "inactive"
        break
      }
      if (length(targets) > 1L) {
        candidates <- targets; issue <- "ambiguous_replacement"; break
      }
      if (targets %in% path) { issue <- "replacement_cycle"; break }
      relations <- c(relations, unique(r$relation[r$orphacode %in% cur & r$target %in% targets]))
      cur <- targets; path <- c(path, cur); steps <- steps + 1L
    }
    data.frame(orphacode = cur, steps = steps,
      relation = if (length(relations)) paste(unique(relations), collapse = "; ") else NA_character_,
      update_issue = issue, path = paste(path, collapse = " -> "),
      candidates = if (length(candidates)) paste(candidates, collapse = ";") else NA_character_,
      stringsAsFactors = FALSE)
  })
  z <- if (length(resolved)) do.call(rbind, resolved) else
    emptydf(orphacode = "character", steps = "integer", relation = "character",
            update_issue = "character", path = "character", candidates = "character")
  z <- z[match(code, keys), , drop = FALSE]
  missing <- is.na(codes) | !nzchar(trimws(as.character(codes)))
  z$update_issue[missing] <- "missing"
  i <- match(z$orphacode, d$orphacode)
  ascdf(data.frame(
    input = as.character(codes), orphacode = z$orphacode, name = d$name[i],
    changed = !is.na(code) & z$orphacode != code, steps = z$steps, relation = z$relation,
    status = d$status[i], update_issue = z$update_issue, path = z$path,
    candidates = z$candidates, stringsAsFactors = FALSE
  ))
}

#' Compare two releases of the nomenclature
#'
#' Lists what changed between two [orphadb] objects (typically two yearly
#' releases obtained with [orphafetch()]): new codes, codes no longer
#' listed, renamed entities, status changes (notably activations and
#' deprecations), classification level changes and changes of preferential
#' parent.
#'
#' @param old,new Two [orphadb] objects.
#'
#' @return An object of class `orphadiff`: a list of data frames `added`,
#'   `removed`, `renamed`, `status`, `level`, `parent`, each keyed by
#'   `orphacode`, plus `tables` and `meta` with both versions. `tables`
#'   contains `added` and `removed` rows for every data table, including
#'   all disorder fields, synonyms, mappings, redirects, classifications,
#'   edges, prevalence and genes. Modified rows appear on both sides;
#'   row order and duplicate identical rows do not constitute a change.
#' @export
#' @examplesIf interactive()
#' d <- orphadiff(orphafetch("2024"), orphafetch("2025"))
#' d
#' d$status
orphadiff <- function(old, new) {
  if (!isdb(old) || !isdb(new)) cli::cli_abort("{.arg old} and {.arg new} must be {.cls orphadb} objects.")
  a <- old$disorders[c("orphacode", "name", "status", "level", "preferential_parent")]
  b <- new$disorders[c("orphacode", "name", "status", "level", "preferential_parent")]
  added <- basetable::addedrows(a, b, by = "orphacode")
  removed <- basetable::removedrows(a, b, by = "orphacode")
  both <- basetable::merge(a, b, by = "orphacode", suffixes = c("_old", "_new"))
  changed <- function(col) {
    x <- both[[paste0(col, "_old")]]; y <- both[[paste0(col, "_new")]]
    k <- !(is.na(x) & is.na(y)) & (is.na(x) | is.na(y) | x != y)
    cols <- unique(c("orphacode", "name_new", paste0(col, c("_old", "_new"))))
    out <- both[k, cols, drop = FALSE]
    names(out)[names(out) == "name_new"] <- if (col == "name") "name_new" else "name"
    ascdf(out)
  }
  table_names <- c("disorders", "synonyms", "redirects", "xrefs", "classifications",
                   "edges", "prevalence", "genes")
  tables <- stats::setNames(lapply(table_names, function(nm) {
    tablediff(old[[nm]], new[[nm]])
  }), table_names)
  structure(list(
    added = ascdf(added), removed = ascdf(removed),
    renamed = changed("name"), status = changed("status"),
    level = changed("level"), parent = changed("preferential_parent"),
    tables = tables,
    meta = list(old = old$meta$version, new = new$meta$version,
                n_old = nrow(a), n_new = nrow(b))
  ), class = "orphadiff")
}

#' @export
print.orphadiff <- function(x, ...) {
  cli::cli_h1("Orphanet nomenclature {x$meta$old} -> {x$meta$new}")
  cli::cli_text("{x$meta$n_old} -> {x$meta$n_new} entities")
  cli::cli_ul(c(
    "{nrow(x$added)} added", "{nrow(x$removed)} removed", "{nrow(x$renamed)} renamed",
    "{nrow(x$status)} status change{?s}", "{nrow(x$level)} level change{?s}",
    "{nrow(x$parent)} preferential parent change{?s}"
  ))
  if (length(x$tables)) {
    for (nm in names(x$tables)) {
      z <- x$tables[[nm]]
      if (nrow(z$added) + nrow(z$removed))
        cli::cli_text("{nm}: {nrow(z$added)} added/changed rows, {nrow(z$removed)} removed/previous rows")
    }
  }
  if (nrow(x$status)) {
    s <- basetable::count(x$status, c("status_old", "status_new"))
    cli::cli_text("")
    print(s)
  }
  invisible(x)
}

# Row-set comparison retaining all fields, including mapping metadata.
tablediff <- function(old, new) {
  columns <- union(names(old), names(new))
  align <- function(x) {
    if (is.null(x)) x <- data.frame()
    for (nm in setdiff(columns, names(x))) x[[nm]] <- rep(NA, nrow(x))
    unique(as.data.frame(x)[columns])
  }
  a <- align(old); b <- align(new)
  difference <- function(x, y) {
    both <- rbind(y, x)
    keep <- !duplicated(both)[nrow(y) + seq_len(nrow(x))]
    ascdf(x[keep, , drop = FALSE])
  }
  list(added = difference(b, a), removed = difference(a, b))
}
