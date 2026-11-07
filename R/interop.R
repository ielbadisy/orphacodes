#' Map ORPHAcodes to other terminologies
#'
#' Returns every alignment recorded by Orphanet between an ORPHAcode and a
#' target terminology, with the relation type (`E` exact, `BTNT` the
#' ORPHAcode is broader than the target, `NTBT` narrower, `ND` not yet
#' decided) and the validation status. Mappings are many-to-many; nothing
#' is collapsed unless you ask for it with `best`.
#'
#' @param codes ORPHAcodes.
#' @param to Target terminology: one of `"ICD10"`, `"ICD11"`, `"OMIM"`,
#'   `"MONDO"`, `"UMLS"`, `"MeSH"`, `"MedDRA"`, `"GARD"` (availability
#'   depends on what the database holds; see `orphasources()`).
#' @param db An [orphadb]; defaults to the registered database.
#' @param relation Optional subset of relation types to keep, for example
#'   `c("E", "BTNT")`.
#' @param validated_only Logical; keep only validated alignments.
#' @param best Logical; keep one target per ORPHAcode, preferring exact
#'   then narrower-target then broader-target relations, validated first.
#'
#' @return A data frame with `orphacode`, `name`, `source`, `code`,
#'   `relation`, `relation_label`, `icd_relation`, `validation`. Codes with
#'   no mapping are kept with `NA` targets so the output can be merged back.
#' @export
#' @examples
#' orphamap("324", to = "ICD10", db = orphamini)
#' orphamap(c("324", "558"), to = "OMIM", db = orphamini, best = TRUE)
orphamap <- function(codes, to = "ICD10", db = NULL, relation = NULL,
                     validated_only = FALSE, best = FALSE) {
  db <- getdb(db)
  to <- normsource(to)
  code <- orphacode(codes)
  x <- db$xrefs[db$xrefs$source == to, , drop = FALSE]
  if (!nrow(x)) cli::cli_warn("The database holds no {to} alignments; see {.fn orphasources}.")
  if (!is.null(relation)) x <- x[x$relation %in% relation, , drop = FALSE]
  if (isTRUE(validated_only)) x <- x[x$validation %in% "Validated", , drop = FALSE]
  q <- data.frame(orphacode = unique(code[!is.na(code)]), stringsAsFactors = FALSE)
  out <- basetable::merge(q, x, by = "orphacode", all.x = TRUE)
  out$source <- to
  out$name <- pick(out$orphacode, db$disorders, "orphacode", "name")
  out$rank <- match(out$relation, c("E", "NTBT", "BTNT", "ND")) * 2 +
    (out$validation %in% "Validated" == FALSE)
  out <- basetable::orderrows(out, c("orphacode", "rank"))
  if (isTRUE(best)) out <- basetable::removeduplicates(out, by = "orphacode")
  out <- out[order(match(out$orphacode, code)), , drop = FALSE]
  ascdf(out[c("orphacode", "name", "source", "code", "relation", "relation_label",
              "icd_relation", "validation")])
}

#' Map codes from another terminology to ORPHAcodes
#'
#' The reverse of [orphamap()]: given ICD-10, OMIM, MONDO (etc.) codes,
#' returns every ORPHAcode aligned to each of them. Because one ICD-10 code
#' typically covers several rare diseases, the result is usually one-to-many
#' and the relation column tells how specific the alignment is.
#'
#' @param codes Codes in the source terminology (for example `"E75.2"`).
#' @param from Source terminology, as in `to` of [orphamap()].
#' @param active_only Logical; drop inactive ORPHAcodes.
#' @inheritParams orphamap
#'
#' @return A data frame with `input`, `source`, `orphacode`, `name`,
#'   `status`, `level`, `relation`, `relation_label`, `validation`.
#' @export
#' @examples
#' orphafrom("E75.2", from = "ICD10", db = orphamini)
orphafrom <- function(codes, from = "ICD10", db = NULL, relation = NULL,
                      validated_only = FALSE, active_only = TRUE) {
  db <- getdb(db)
  from <- normsource(from)
  x <- db$xrefs[db$xrefs$source == from, , drop = FALSE]
  if (!is.null(relation)) x <- x[x$relation %in% relation, , drop = FALSE]
  if (isTRUE(validated_only)) x <- x[x$validation %in% "Validated", , drop = FALSE]
  key <- normcode(codes)
  x$key <- normcode(x$code)
  q <- data.frame(input = as.character(codes), key = key, stringsAsFactors = FALSE)
  q <- q[!duplicated(q$key), , drop = FALSE]
  out <- basetable::merge(q, x, by = "key", all.x = TRUE)
  d <- db$disorders
  i <- match(out$orphacode, d$orphacode)
  out$name <- d$name[i]
  out$status <- d$status[i]
  out$level <- d$level[i]
  out$source <- from
  if (isTRUE(active_only)) out <- out[is.na(i) | d$active[i], , drop = FALSE]
  out <- out[order(match(out$key, key), out$relation), , drop = FALSE]
  ascdf(out[c("input", "source", "orphacode", "name", "status", "level",
              "relation", "relation_label", "validation")])
}

# Normalise a foreign code for matching: upper case, no spaces, ICD dots kept.
normcode <- function(x) {
  x <- toupper(trimws(as.character(x)))
  gsub("[[:space:]]", "", x)
}

#' Terminologies available in a database
#'
#' @param db An [orphadb]; defaults to the registered database.
#' @return A data frame with `source`, `n_mappings`, `n_orphacodes` and the
#'   breakdown by relation type.
#' @export
#' @examples
#' orphasources(orphamini)
orphasources <- function(db = NULL) {
  db <- getdb(db)
  x <- db$xrefs
  if (!nrow(x)) return(ascdf(emptydf(source = "character", n_mappings = "integer", n_orphacodes = "integer")))
  n <- basetable::count(x, "source", sort = FALSE, name = "n_mappings")
  u <- basetable::aggregate(x, by = "source", value = "orphacode", fun = function(v) length(unique(v)))
  names(u)[2] <- "n_orphacodes"
  r <- basetable::count(x, c("source", "relation"), sort = FALSE)
  r <- basetable::towide(r, names = "relation", values = "n", fill = 0L)
  out <- basetable::merge(basetable::merge(n, u, by = "source"), r, by = "source", all.x = TRUE)
  out <- basetable::orderrows(out, "n_mappings", decreasing = TRUE)
  out[is.na(out)] <- 0L
  ascdf(out)
}
