#' orphacodes: rare disease nomenclature, coding and registry tools
#'
#' `orphacodes` turns the Orphanet nomenclature into a programmable,
#' version-aware data management layer for rare disease research and
#' registries. It is organised in five modules:
#'
#' * **Data**: [orphafetch()] downloads a pinned release of the Orphanet
#'   nomenclature files for coding and the free Orphadata products into a
#'   cached [orphadb] object; [orphasave()] and [orphaload()] freeze it for
#'   reproducible analyses; [orphause()] sets the default database.
#' * **Nomenclature**: [orphacode()], [orphalookup()], [orphasearch()],
#'   [orphavalidate()], [orphastandardize()], and the hierarchy helpers
#'   [orphaparents()], [orphachildren()], [orphaancestors()],
#'   [orphadescendants()] and [orphagroup()].
#' * **Interoperability**: [orphamap()] and [orphafrom()] align ORPHAcodes
#'   with ICD-10, ICD-11, OMIM, MONDO, UMLS, MeSH, MedDRA and GARD without
#'   collapsing one-to-many relations.
#' * **Versioning**: [orphahistory()], [orphaupdate()] and [orphadiff()]
#'   follow deprecated and obsolete codes across releases.
#' * **Registries and epidemiology**: [orpharedcap()], [orphaqc()],
#'   [orphaharmonize()], [orphaaggregate()] and [orphaprevref()].
#'
#' All Orphanet data are distributed under the Creative Commons Attribution
#' 4.0 licence by INSERM. Cite Orphanet and Orphadata as indicated by
#' [orphacite()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.orpha <- new.env(parent = emptyenv())
