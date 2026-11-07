#' The `orphadb` object
#'
#' An `orphadb` is a list of plain data frames holding one release of the
#' Orphanet nomenclature, with provenance in `meta`. Every function in the
#' package takes a `db` argument that defaults to the database registered
#' with [orphause()] (or by [orphafetch()] and [orphaload()]). The bundled
#' [orphamini] is a small `orphadb` for examples and tests.
#'
#' @section Tables:
#' * `disorders`: one row per clinical entity: `orphacode`, `name`,
#'   `status` (`"Active"`, `"Active: Historic"`, `"Inactive: Deprecated"`,
#'   `"Inactive: Obsolete"`, `"Inactive: Non rare disease in Europe"`),
#'   `active`, `flag`, `type` (typology), `level` (`"Group of disorders"`,
#'   `"Disorder"`, `"Subtype of disorder"`), `definition`, `note`,
#'   `expert_link`, `aggregation_code`, `aggregation_name`,
#'   `aggregation_status`, `preferential_parent`,
#'   `preferential_parent_name`.
#' * `synonyms`: `orphacode`, `synonym`.
#' * `redirects`: `orphacode`, `target`, `target_name`, `relation`
#'   (`"Moved to"` for deprecated entities, `"Referred to"` for obsolete
#'   ones).
#' * `xrefs`: `orphacode`, `source` (`"ICD10"`, `"ICD11"`, `"OMIM"`,
#'   `"MONDO"`, `"UMLS"`, `"MeSH"`, `"MedDRA"`, `"GARD"`), `code`,
#'   `relation` (`"E"` exact, `"BTNT"` ORPHAcode broader than target,
#'   `"NTBT"` ORPHAcode narrower than target, `"ND"` not decided),
#'   `relation_label`, `icd_relation`, `validation`, `url`.
#' * `classifications`: `classification_id`, `classification_code`,
#'   `classification`, `root`.
#' * `edges`: `classification_id`, `parent`, `child`, `child_name`,
#'   `child_type`. An entity may appear under several parents and in
#'   several classifications.
#' * `prevalence`: `orphacode`, `type`, `qualification`, `class`, `value`
#'   (per 100,000 for prevalence and incidence types, number of cases or
#'   families otherwise), `geographic`, `validation`, `source`.
#' * `genes`: `orphacode`, `gene_symbol`, `gene_name`, `hgnc`,
#'   `omim_gene`, `ensembl`, `locus`, `association_type`,
#'   `association_status`, `source`.
#' * `meta`: `version` (release label), `lang`, `extraction_date`,
#'   `db_version`, `fetched` (time stamp), `sources` (named URLs),
#'   `licence`.
#'
#' @name orphadb
#' @aliases orphadb-class
NULL

newdb <- function(disorders, synonyms, redirects, xrefs = NULL,
                  classifications = NULL, edges = NULL, prevalence = NULL,
                  genes = NULL, meta = list()) {
  xrefs <- xrefs %||% emptydf(orphacode = "character", source = "character",
    code = "character", relation = "character", relation_label = "character",
    icd_relation = "character", validation = "character", url = "character")
  classifications <- classifications %||% emptydf(classification_id = "character",
    classification_code = "character", classification = "character", root = "character")
  edges <- edges %||% emptydf(classification_id = "character", parent = "character",
    child = "character", child_name = "character", child_type = "character")
  prevalence <- prevalence %||% emptydf(orphacode = "character", type = "character",
    qualification = "character", class = "character", value = "numeric",
    geographic = "character", validation = "character", source = "character")
  genes <- genes %||% emptydf(orphacode = "character", gene_symbol = "character",
    gene_name = "character", hgnc = "character", omim_gene = "character",
    ensembl = "character", locus = "character", association_type = "character",
    association_status = "character", source = "character")
  if (!"preferential_parent" %in% names(disorders)) {
    disorders$preferential_parent <- NA_character_
    disorders$preferential_parent_name <- NA_character_
  }
  structure(
    list(disorders = disorders, synonyms = synonyms, redirects = redirects,
         xrefs = xrefs, classifications = classifications, edges = edges,
         prevalence = prevalence, genes = genes, meta = meta),
    class = "orphadb"
  )
}

#' @export
print.orphadb <- function(x, ...) {
  m <- x$meta
  cli::cli_h1("Orphanet nomenclature database")
  cli::cli_text("Release {.strong {m$version %||% 'unknown'}} ({m$lang %||% 'en'}), extracted {m$extraction_date %||% 'unknown'}")
  d <- x$disorders
  cli::cli_bullets(c(
    "*" = "{nrow(d)} clinical entit{?y/ies}: {sum(d$active, na.rm = TRUE)} active, {sum(!d$active, na.rm = TRUE)} inactive",
    "*" = "{nrow(x$synonyms)} synonym{?s}, {nrow(x$redirects)} redirect{?s}",
    "*" = "{nrow(x$xrefs)} cross-reference{?s} to {length(unique(x$xrefs$source))} system{?s}{if (nrow(x$xrefs)) paste0(' (', paste(sort(unique(x$xrefs$source)), collapse = ', '), ')') else ''}",
    "*" = "{nrow(x$classifications)} classification{?s}, {nrow(x$edges)} parent-child edge{?s}",
    "*" = "{nrow(x$prevalence)} prevalence record{?s}, {nrow(x$genes)} gene association{?s}"
  ))
  invisible(x)
}

#' @export
summary.orphadb <- function(object, ...) {
  d <- object$disorders
  out <- list(
    version = object$meta$version,
    extraction_date = object$meta$extraction_date,
    entities = nrow(d),
    status = table(d$status, useNA = "ifany"),
    level = table(d$level, useNA = "ifany"),
    xref_sources = table(object$xrefs$source),
    classifications = nrow(object$classifications)
  )
  class(out) <- "summary.orphadb"
  out
}

#' @export
print.summary.orphadb <- function(x, ...) {
  cat("Orphanet nomenclature", x$version, "extracted", format(x$extraction_date), "\n")
  cat("Entities:", x$entities, "\n\nStatus:\n")
  print(x$status)
  cat("\nClassification level:\n")
  print(x$level)
  if (length(x$xref_sources)) {
    cat("\nCross-references:\n")
    print(x$xref_sources)
  }
  cat("\nClassifications:", x$classifications, "\n")
  invisible(x)
}

isdb <- function(x) inherits(x, "orphadb")

#' Register, retrieve, save and load the working database
#'
#' `orphause()` registers an [orphadb] as the default used by every function
#' whose `db` argument is left `NULL`. `orphacurrent()` returns it, falling
#' back to the bundled [orphamini] (with a message) when nothing has been
#' registered. `orphasave()` writes a database to a single `.rds` file and
#' `orphaload()` reads it back and registers it, which is the recommended way
#' to freeze the nomenclature release used by an analysis.
#'
#' @param db An [orphadb] object. For `orphause()`, `NULL` clears the
#'   registration.
#' @param path Path of an `.rds` file.
#' @param use Logical; register the loaded database as the default.
#'
#' @return `orphause()` returns the previously registered database,
#'   invisibly. `orphacurrent()` returns the registered database.
#'   `orphasave()` returns `path` invisibly. `orphaload()` returns the
#'   database.
#' @export
#' @examples
#' orphause(orphamini)
#' orphacurrent()$meta$version
#' f <- tempfile(fileext = ".rds")
#' orphasave(orphamini, f)
#' db <- orphaload(f)
#' identical(db$disorders, orphamini$disorders)
orphause <- function(db) {
  if (!is.null(db) && !isdb(db)) cli::cli_abort("{.arg db} must be an {.cls orphadb}.")
  old <- .orpha$db
  .orpha$db <- db
  invisible(old)
}

#' @rdname orphause
#' @export
orphacurrent <- function() {
  db <- .orpha$db
  if (is.null(db)) {
    if (!isTRUE(.orpha$warned)) {
      cli::cli_alert_info("No database registered; using the bundled {.val orphamini} demo subset. Call {.fn orphafetch} for the full nomenclature.")
      .orpha$warned <- TRUE
    }
    db <- orphacodes::orphamini
  }
  db
}

getdb <- function(db) {
  if (is.null(db)) return(orphacurrent())
  if (!isdb(db)) cli::cli_abort("{.arg db} must be an {.cls orphadb} (see {.fn orphafetch}).")
  db
}

#' @rdname orphause
#' @export
orphasave <- function(db, path) {
  db <- getdb(db)
  saveRDS(db, path, compress = "xz")
  invisible(path)
}

#' @rdname orphause
#' @export
orphaload <- function(path, use = TRUE) {
  db <- readRDS(path)
  if (!isdb(db)) cli::cli_abort("{.path {path}} does not contain an {.cls orphadb}.")
  if (isTRUE(use)) orphause(db)
  db
}

#' How to cite Orphanet data
#'
#' Returns the citation strings requested by Orphanet and Orphadata, filled
#' with the release used.
#'
#' @param db An [orphadb]; defaults to the registered database.
#' @return A named character vector with elements `orphanet` and `orphadata`.
#' @export
#' @examples
#' orphacite(orphamini)
orphacite <- function(db = NULL) {
  db <- getdb(db)
  c(
    orphanet = paste0("Orphanet: an online rare disease and orphan drug data base. ",
                      "INSERM 1999. Available on https://www.orpha.net. Accessed ",
                      format(as.Date(db$meta$fetched %||% Sys.time()), "%Y-%m-%d"), "."),
    orphadata = paste0("Orphadata: Free access data from Orphanet. INSERM 1999. ",
                       "Available on https://www.orphadata.com. Data version ",
                       db$meta$db_version %||% "unknown", " (",
                       db$meta$version %||% "unknown", ").")
  )
}
