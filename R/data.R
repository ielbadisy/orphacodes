#' Bundled subset of the Orphanet nomenclature
#'
#' An [orphadb] restricted to three Orphanet classifications (rare cardiac
#' diseases, rare neurological diseases, rare inborn errors of metabolism)
#' and a few example entities; functions mention once when a code falls
#' outside this subset. It was formerly restricted to rare cardiac
#' diseases (classification 146) plus a few entities used in examples
#' (Fabry disease, Marfan syndrome, a deprecated pontocerebellar hypoplasia
#' code and the top of the neurological classification). Built from the
#' 2026 release of the nomenclature files for coding and the Orphadata
#' products; see `data-raw/orphamini.R`. Use [orphafetch()] for the full
#' nomenclature.
#'
#' @format An [orphadb]; `orphamini$meta` records the release.
#' @source Orphanet, \url{https://www.orphadata.com}, CC-BY-4.0.
"orphamini"

#' Simulated rare-disease registry extract
#'
#' A small simulated registry (120 rows) coded with `orphamini` codes and
#' names, with the defects met in practice: synonyms and typos in the
#' free-text diagnosis, ORPHAcodes written with various prefixes,
#' deprecated codes, group-level codes, missing and malformed codes, and
#' one duplicated patient identifier. See `data-raw/orphademo.R`.
#'
#' @format A data frame with columns `patient_id`, `sex`, `birth_year`,
#'   `diagnosis` (free text) and `diagnosis_code` (ORPHAcode as typed).
"orphademo"
