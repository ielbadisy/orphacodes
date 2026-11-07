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
#' @return `orphahistory()`: a data frame with `orphacode`, `name`,
#'   `status`, `relation`, `target`, `target_name`, `target_status`.
#'   `orphaupdate()`: a data frame with `input`, `orphacode` (the updated
#'   code, or the input when no replacement applies), `name`, `changed`,
#'   `steps`, `relation`, `status`.
#' @export
#' @examples
#' orphahistory(c("166068", "324"), db = orphamini)
#' orphaupdate(c("166068", "324"), db = orphamini)
orphahistory <- function(codes, db = NULL) {
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
  cur <- code
  steps <- integer(length(code))
  rel <- rep(NA_character_, length(code))
  for (k in seq_len(10L)) {
    j <- match(cur, r$orphacode)
    hit <- !is.na(j) & !d$active[match(cur, d$orphacode)] %in% TRUE
    if (!any(hit)) break
    rel[hit & is.na(rel)] <- r$relation[j[hit & is.na(rel)]]
    cur[hit] <- r$target[j[hit]]
    steps[hit] <- steps[hit] + 1L
  }
  i <- match(cur, d$orphacode)
  ascdf(data.frame(
    input = as.character(codes), orphacode = cur, name = d$name[i],
    changed = !is.na(code) & cur != code, steps = steps, relation = rel,
    status = d$status[i], stringsAsFactors = FALSE
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
#'   `orphacode`, plus `meta` with both versions.
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
  structure(list(
    added = ascdf(added), removed = ascdf(removed),
    renamed = changed("name"), status = changed("status"),
    level = changed("level"), parent = changed("preferential_parent"),
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
  if (nrow(x$status)) {
    s <- basetable::count(x$status, c("status_old", "status_new"))
    cli::cli_text("")
    print(s)
  }
  invisible(x)
}
