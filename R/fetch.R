rdcode_repo <- "orphanet-rare-diseases-issues/RD-CODE"
orphadata_base <- "https://www.orphadata.com/data/xml"

#' Location of the orphacodes download cache
#'
#' Downloaded Orphanet files and parsed databases are kept in a persistent
#' cache so that a release is fetched at most once. The location is, in
#' order of precedence, the `orphacodes.cache_dir` option, the
#' `ORPHACODES_CACHE_DIR` environment variable, or the user cache directory
#' returned by [tools::R_user_dir()].
#'
#' @param clear Logical; remove every cached file.
#' @param create Logical; create the directory if it does not exist.
#'
#' @return The cache directory path (invisibly when `clear = TRUE`).
#' @export
#' @examples
#' withr_like <- options(orphacodes.cache_dir = tempfile())
#' orphacache(create = FALSE)
#' options(withr_like)
orphacache <- function(clear = FALSE, create = TRUE) {
  dir <- getOption("orphacodes.cache_dir")
  if (is.null(dir)) dir <- Sys.getenv("ORPHACODES_CACHE_DIR", unset = NA_character_)
  if (is.na(dir) || !nzchar(dir)) dir <- tools::R_user_dir("orphacodes", which = "cache")
  if (isTRUE(clear) && dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
    cli::cli_alert_success("Cleared {.path {dir}}.")
    return(invisible(dir))
  }
  if (isTRUE(create) && !dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

ghapi <- function(path, query = NULL) {
  url <- paste0("https://api.github.com/", path)
  if (length(query)) {
    url <- paste0(url, "?", paste(names(query), utils::URLencode(as.character(query), reserved = TRUE),
                                   sep = "=", collapse = "&"))
  }
  h <- c(Accept = "application/vnd.github+json", `User-Agent` = "orphacodes-R")
  pat <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
  if (nzchar(pat)) h <- c(h, Authorization = paste("Bearer", pat))
  con <- url(url, headers = h)
  on.exit(close(con), add = TRUE)
  txt <- tryCatch(readLines(con, warn = FALSE), error = function(e) {
    cli::cli_abort(c("GitHub API request failed for {.url {url}}.",
                     "i" = "Check your connection; unauthenticated requests are limited to 60 per hour (set {.envvar GITHUB_PAT} to raise the limit).",
                     "x" = conditionMessage(e)))
  })
  jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
}

#' Available releases of the Orphanet nomenclature files for coding
#'
#' Lists the yearly releases of the Orphanet nomenclature pack published in
#' the RD-CODE GitHub repository, which is where [orphafetch()] pins a
#' `version`. Requires network access.
#'
#' @return A data frame with `version` (year), `date` of the last commit of
#'   that year and the commit `sha`.
#' @export
#' @examplesIf interactive()
#' orphaversions()
orphaversions <- function() {
  commits <- ghapi(paste0("repos/", rdcode_repo, "/commits"), list(per_page = 100))
  dates <- as.Date(substr(commits$commit$committer$date, 1, 10))
  years <- format(dates, "%Y")
  keep <- !duplicated(years)
  ascdf(data.frame(version = years[keep], date = dates[keep], sha = commits$sha[keep],
                   stringsAsFactors = FALSE)[order(years[keep]), ])
}

resolveref <- function(version) {
  if (identical(version, "latest")) return(list(ref = "master", label = NULL))
  if (grepl("^[0-9a-f]{7,40}$", version)) return(list(ref = version, label = version))
  if (!grepl("^[0-9]{4}$", version)) {
    cli::cli_abort("{.arg version} must be {.val latest}, a year such as {.val 2025}, or a commit SHA.")
  }
  res <- ghapi(paste0("repos/", rdcode_repo, "/commits"),
               list(until = paste0(version, "-12-31T23:59:59Z"), per_page = 1))
  if (!length(res) || is.null(res$sha)) cli::cli_abort("No release found for {.val {version}}.")
  d <- substr(res$commit$committer$date, 1, 10)
  if (substr(d, 1, 4) != version) {
    cli::cli_warn("No commit in {version}; using the last release before it ({d}).")
  }
  list(ref = res$sha[1], label = version)
}

ghlist <- function(ref, dir) {
  res <- ghapi(paste0("repos/", rdcode_repo, "/contents/", utils::URLencode(dir)), list(ref = ref))
  if (is.null(res$name)) character() else res$name
}

rawurl <- function(ref, path) {
  paste0("https://raw.githubusercontent.com/", rdcode_repo, "/", ref, "/", utils::URLencode(path))
}

getfile <- function(url, dest, refresh = FALSE, quiet = TRUE) {
  if (file.exists(dest) && file.size(dest) > 0 && !refresh) return(dest)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest, ".part")
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = quiet)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) {
    unlink(tmp)
    cli::cli_abort("Download failed: {.url {url}}")
  }
  file.rename(tmp, dest)
  dest
}

#' Download a release of the Orphanet nomenclature
#'
#' Downloads, parses and caches one release of the Orphanet nomenclature
#' files for coding (RD-CODE pack: nomenclature, ICD-10 and ICD-11
#' alignments, classifications, linearisation) and, optionally, the free
#' Orphadata products for the other cross-references, prevalence and genes.
#' The result is an [orphadb] registered as the default database.
#'
#' The nomenclature pack is pinned to a yearly release through the RD-CODE
#' GitHub history (see [orphaversions()]), so `orphafetch("2025")` returns
#' the same tables today and in five years. The Orphadata products are only
#' distributed as "current" files; their extraction date is recorded in
#' `meta` and the resulting database can be frozen with [orphasave()].
#'
#' @param version `"latest"`, a release year such as `"2025"`, or a commit
#'   SHA of the RD-CODE repository.
#' @param lang Two-letter language of the nomenclature: one of `"en"`,
#'   `"fr"`, `"de"`, `"es"`, `"it"`, `"nl"`, `"pt"`, `"pl"`, `"cs"`.
#' @param include Character vector of optional Orphadata products to add:
#'   `"xrefs"` (OMIM, MONDO, UMLS, MeSH, MedDRA, GARD alignments),
#'   `"prevalence"` and `"genes"`. Use `character()` for the pack only.
#' @param classifications Logical; download the 33 classification files
#'   (needed for [orphaparents()] and friends).
#' @param refresh Logical; re-download even if cached.
#' @param use Logical; register the database with [orphause()].
#' @param quiet Logical; suppress progress messages.
#'
#' @return An [orphadb].
#' @seealso [orphabuild()] to assemble a database from local files.
#' @export
#' @examplesIf interactive()
#' db <- orphafetch("2025")
#' db
orphafetch <- function(version = "latest", lang = "en",
                       include = c("xrefs", "prevalence", "genes"),
                       classifications = TRUE, refresh = FALSE, use = TRUE,
                       quiet = FALSE) {
  lang <- tolower(lang)
  include <- if (length(include)) match.arg(include, c("xrefs", "prevalence", "genes"), several.ok = TRUE) else character()
  r <- resolveref(version)
  cdir <- file.path(orphacache(), paste0(r$label %||% "latest", "-", lang))
  rds <- file.path(cdir, paste0("orphadb-", paste(c(include, if (classifications) "cl"), collapse = "-"), ".rds"))
  if (file.exists(rds) && !refresh) {
    db <- readRDS(rds)
    if (!quiet) cli::cli_alert_success("Loaded cached release {.strong {db$meta$version}} ({lang}) from {.path {cdir}}.")
    if (use) orphause(db)
    return(db)
  }
  if (!quiet) cli::cli_alert_info("Fetching Orphanet nomenclature pack ({r$label %||% 'latest'}, {lang}) from RD-CODE ...")
  files <- list()
  src <- character()
  nom <- grep(paste0("ORPHAnomenclature_", lang, "(_[0-9]{4})?\\.xml$"), ghlist(r$ref, "ORPHAnomenclature"), value = TRUE)
  if (!length(nom)) cli::cli_abort("No nomenclature file for language {.val {lang}} in release {.val {version}}.")
  files$nomenclature <- getfile(rawurl(r$ref, paste0("ORPHAnomenclature/", nom[1])), file.path(cdir, nom[1]), refresh)
  src["nomenclature"] <- rawurl(r$ref, paste0("ORPHAnomenclature/", nom[1]))
  for (icd in c("ICD10", "ICD11")) {
    dir <- paste0("ORPHA_", icd, "_mapping")
    f <- grep(paste0("_", lang, "(_[0-9]{4})?\\.xml$"), tryCatch(ghlist(r$ref, dir), error = function(e) character()), value = TRUE)
    if (length(f)) {
      files[[tolower(icd)]] <- getfile(rawurl(r$ref, paste0(dir, "/", f[1])), file.path(cdir, f[1]), refresh)
      src[tolower(icd)] <- rawurl(r$ref, paste0(dir, "/", f[1]))
    }
  }
  f <- grep(paste0("ORPHAlinearisation_(", lang, "|en)(_[0-9]{4})?\\.xml$"),
            tryCatch(ghlist(r$ref, "ORPHAlinearisation"), error = function(e) character()), value = TRUE)
  if (length(f)) {
    f <- f[order(!grepl(paste0("_", lang, "_"), f))][1]
    files$linearisation <- getfile(rawurl(r$ref, paste0("ORPHAlinearisation/", f)), file.path(cdir, f), refresh)
    src["linearisation"] <- rawurl(r$ref, paste0("ORPHAlinearisation/", f))
  }
  if (isTRUE(classifications)) {
    cf <- tryCatch(ghlist(r$ref, paste0("Classifications/", lang)), error = function(e) character())
    cf <- grep("\\.xml$", cf, value = TRUE)
    if (length(cf)) {
      if (!quiet) cli::cli_alert_info("Downloading {length(cf)} classification files ...")
      files$classifications <- vapply(cf, function(x) {
        getfile(rawurl(r$ref, paste0("Classifications/", lang, "/", x)), file.path(cdir, "classifications", x), refresh)
      }, character(1))
      src["classifications"] <- rawurl(r$ref, paste0("Classifications/", lang, "/"))
    }
  }
  if (length(include)) {
    if (!quiet) cli::cli_alert_info("Downloading Orphadata products ({paste(include, collapse = ', ')}) ...")
    prod <- c(xrefs = "product1", prevalence = "product9_prev", genes = "product6")
    for (p in include) {
      u <- paste0(orphadata_base, "/", lang, "_", prod[[p]], ".xml")
      dest <- file.path(cdir, paste0(lang, "_", prod[[p]], ".xml"))
      ok <- tryCatch({ getfile(u, dest, refresh); TRUE }, error = function(e) {
        if (lang != "en") {
          cli::cli_warn("No {lang} {prod[[p]]} on Orphadata; using the English file.")
          u <<- paste0(orphadata_base, "/en_", prod[[p]], ".xml")
          dest <<- file.path(cdir, paste0("en_", prod[[p]], ".xml"))
          getfile(u, dest, refresh); TRUE
        } else FALSE
      })
      if (ok) { files[[p]] <- dest; src[p] <- u }
    }
  }
  db <- orphabuild(
    nomenclature = files$nomenclature, icd10 = files$icd10, icd11 = files$icd11,
    linearisation = files$linearisation, classifications = files$classifications,
    product1 = files$xrefs, prevalence = files$prevalence, genes = files$genes,
    version = r$label, lang = lang, sources = src, quiet = quiet
  )
  saveRDS(db, rds, compress = "xz")
  if (!quiet) cli::cli_alert_success("Release {.strong {db$meta$version}} cached in {.path {cdir}}.")
  if (use) orphause(db)
  db
}

#' Build an Orphanet database from local files
#'
#' Assembles an [orphadb] from Orphanet XML files already on disk, which is
#' what [orphafetch()] does after downloading. Use it for air-gapped
#' systems, for files obtained directly from Orphanet, or for reproducible
#' pipelines that vendor the source files.
#'
#' @param nomenclature Path to `ORPHAnomenclature_<lang>.xml` (nomenclature
#'   pack) or, as a fallback, to an Orphadata `product1` file.
#' @param icd10,icd11 Paths to the pack alignment files (optional).
#' @param linearisation Path to the linearisation file (optional).
#' @param classifications Character vector of classification file paths
#'   (optional).
#' @param product1 Path to the Orphadata cross-referencing product for the
#'   non-ICD alignments (optional).
#' @param prevalence,genes Paths to Orphadata `product9_prev` and
#'   `product6` (optional).
#' @param version Release label stored in `meta$version`; defaults to the
#'   extraction year of the nomenclature file.
#' @param lang Language code stored in `meta`.
#' @param sources Optional named character vector of source URLs stored in
#'   `meta$sources`.
#' @param quiet Logical; suppress messages.
#'
#' @return An [orphadb].
#' @export
#' @examples
#' \dontrun{
#' db <- orphabuild(
#'   nomenclature = "ORPHAnomenclature_en.xml",
#'   icd10 = "ORPHA_ICD10_mapping_en.xml",
#'   classifications = list.files("Classifications/en", full.names = TRUE)
#' )
#' }
orphabuild <- function(nomenclature, icd10 = NULL, icd11 = NULL,
                       linearisation = NULL, classifications = NULL,
                       product1 = NULL, prevalence = NULL, genes = NULL,
                       version = NULL, lang = "en", sources = NULL,
                       quiet = FALSE) {
  if (!quiet) cli::cli_alert_info("Parsing nomenclature ...")
  n <- parsenomenclature(nomenclature)
  xr <- list()
  for (p in c(icd10, icd11)) xr[[length(xr) + 1L]] <- parsexrefs(p)$xrefs
  pmeta <- NULL
  if (!is.null(product1)) {
    if (!quiet) cli::cli_alert_info("Parsing cross-references ...")
    px <- parsexrefs(product1)
    pmeta <- px$meta
    x <- px$xrefs
    have <- unique(unlist(lapply(xr, function(z) z$source)))
    xr[[length(xr) + 1L]] <- x[!x$source %in% have, , drop = FALSE]
  }
  xrefs <- rowbind(xr)
  if (!is.null(xrefs)) xrefs <- ascdf(xrefs[!duplicated(xrefs[c("orphacode", "source", "code")]), ])
  cls <- edges <- NULL
  if (length(classifications)) {
    if (!quiet) cli::cli_alert_info("Parsing {length(classifications)} classification{?s} ...")
    parsed <- lapply(classifications, parseclassification)
    cls <- rowbind(lapply(parsed, `[[`, "classification"))
    edges <- rowbind(lapply(parsed, `[[`, "edges"))
  }
  d <- n$disorders
  if (!is.null(linearisation)) {
    li <- parselinearisation(linearisation)$linearisation
    d$preferential_parent <- pick(d$orphacode, li, "orphacode", "preferential_parent")
    d$preferential_parent_name <- pick(d$orphacode, li, "orphacode", "preferential_parent_name")
  } else {
    d$preferential_parent <- NA_character_
    d$preferential_parent_name <- NA_character_
  }
  pv <- if (!is.null(prevalence)) parseprevalence(prevalence)$prevalence
  gn <- if (!is.null(genes)) parsegenes(genes)$genes
  meta <- list(
    version = version %||% format(n$meta$extraction_date, "%Y"),
    lang = lang,
    extraction_date = n$meta$extraction_date,
    db_version = n$meta$db_version,
    orphadata_extraction_date = pmeta$extraction_date,
    fetched = Sys.time(),
    sources = sources,
    licence = n$meta$licence %||% "CC-BY-4.0",
    package_version = as.character(utils::packageVersion("orphacodes"))
  )
  newdb(disorders = d, synonyms = n$synonyms, redirects = n$redirects,
        xrefs = xrefs, classifications = cls, edges = edges,
        prevalence = pv, genes = gn, meta = meta)
}
