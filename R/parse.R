# Parsers for the Orphanet XML products. All of them return plain data
# frames with character ORPHAcodes (no prefix), using vectorised xml2 XPath
# queries so that even the 50 MB cross-referencing product parses in a few
# seconds.

xmlmeta <- function(doc) {
  root <- xml2::xml_root(doc)
  date <- xml2::xml_attr(root, "ExtractionDate")
  if (is.na(date)) date <- xml2::xml_attr(root, "date")
  list(
    extraction_date = as.Date(substr(date, 1, 10)),
    db_version = xml2::xml_attr(root, "version"),
    copyright = xml2::xml_attr(root, "copyright"),
    licence = xml2::xml_text(xml2::xml_find_first(
      doc, "/JDBOR/Availability/Licence/ShortIdentifier"))
  )
}

t1 <- function(nodes, xpath) {
  xml2::xml_text(xml2::xml_find_first(nodes, xpath))
}

readxml <- function(path) {
  if (!file.exists(path)) cli::cli_abort("File not found: {.path {path}}.")
  doc <- xml2::read_xml(path, options = c("NOBLANKS", "HUGE"))
  legacynames(doc)
}

# Releases up to 2019 use an older schema: OrphaNumber for OrphaCode,
# GroupOfType for ClassificationLevel, OrphanumberAggregation inside the
# aggregation level, and InDisorder/OutDisorder for RootDisorder/
# TargetDisorder in the associations. Rename those elements once so that
# every parser works on one schema.
legacynames <- function(doc) {
  old <- c("OrphaNumber", "GroupOfType", "OrphanumberAggregation", "OutDisorder", "InDisorder")
  new <- c("OrphaCode", "ClassificationLevel", "OrphaCode", "TargetDisorder", "RootDisorder")
  for (i in seq_along(old)) {
    n <- xml2::xml_find_all(doc, paste0("//", old[i]))
    if (length(n)) xml2::xml_set_name(n, new[i])
  }
  doc
}

# Orphanet identifiers are language independent; the labels are not.
# Status, level, type and relation are therefore decoded from FlagValue and
# element ids into canonical English labels, whatever the language of the
# file, so that every function behaves identically across languages.
statusfromflag <- function(flag) {
  f <- suppressWarnings(as.integer(flag))
  ifelse(is.na(f) | f %in% c(1L, 129L), "Active",
  ifelse(f == 513L, "Active: Historic",
  ifelse(bitwAnd(f, 256L) > 0L, "Inactive: Deprecated",
  ifelse(bitwAnd(f, 32L) > 0L, "Inactive: Non rare disease in Europe",
  ifelse(bitwAnd(f, 16L) > 0L | bitwAnd(f, 1024L) > 0L, "Inactive: Obsolete",
  ifelse(bitwAnd(f, 8192L) > 0L, "Inactive", "Active"))))))
}

orphaids <- list(
  level = c("36540" = "Group of disorders", "36547" = "Disorder", "36554" = "Subtype of disorder"),
  type = c("21394" = "Disease", "21401" = "Malformation syndrome", "21450" = "Clinical subtype",
           "21436" = "Clinical group", "36561" = "Category",
           "21429" = "Particular clinical situation in a disease or syndrome",
           "21415" = "Morphological anomaly", "21443" = "Etiological subtype",
           "21408" = "Biological anomaly", "21422" = "Clinical syndrome",
           "21457" = "Histopathological subtype"),
  association = c("21471" = "Moved to", "27341" = "Referred to", "21485" = "Preferential parent"),
  relation = c("21527" = "E", "21534" = "NTBT", "21541" = "BTNT", "21576" = "ND"),
  icd = c("21604" = "Attributed code", "21590" = "Inclusion term", "21597" = "Index term",
          "21583" = "Specific code"),
  validation = c("21611" = "Validated", "21618" = "Not yet validated")
)

# Decode an id into its canonical label, falling back to the localised
# label when the id is unknown.
decode <- function(nodes, xpath, table, label_xpath = paste0(xpath, "/Name")) {
  id <- xml2::xml_attr(xml2::xml_find_first(nodes, xpath), "id")
  out <- unname(table[id])
  miss <- is.na(out)
  if (any(miss)) out[miss] <- t1(nodes, label_xpath)[miss]
  out
}

# Orphanet nomenclature file for coding (ORPHAnomenclature_<lang>.xml) or,
# as a fallback, the Orphadata product1 file: both carry the same core.
parsenomenclature <- function(path) {
  doc <- readxml(path)
  meta <- xmlmeta(doc)
  d <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder")
  code <- t1(d, "OrphaCode")
  flag <- t1(d, "FlagValue")
  if (all(is.na(flag))) {
    # product1 encodes the status as a list of flags: sum them per entity
    fv <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/DisorderFlagList/DisorderFlag/Value")
    fcode <- t1(fv, "../../../OrphaCode")
    fsum <- tapply(suppressWarnings(as.integer(xml2::xml_text(fv))), fcode, sum)
    flag <- unname(fsum[code])
    flag[is.na(flag)] <- 1L
  }
  level <- decode(d, "ClassificationLevel", orphaids$level)
  if (all(is.na(level))) level <- decode(d, "DisorderGroup", orphaids$level)
  disorders <- data.frame(
    orphacode = code,
    name = t1(d, "Name"),
    status = statusfromflag(flag),
    flag = suppressWarnings(as.integer(flag)),
    type = decode(d, "DisorderType", orphaids$type),
    level = level,
    definition = t1(d, "SummaryInformationList/SummaryInformation/TextSectionList/TextSection[TextSectionType/Name='Definition']/Contents"),
    note = t1(d, "SummaryInformationList/SummaryInformation/TextAuto/Info"),
    expert_link = t1(d, "ExpertLink"),
    aggregation_code = t1(d, "AggregationLevelSection/AggregationLevelList/AggregationLevel/OrphaCode"),
    aggregation_name = t1(d, "AggregationLevelSection/AggregationLevelList/AggregationLevel/PreferredTerm"),
    aggregation_status = t1(d, "AggregationLevelSection/AggregationLevelList/AggregationLevel/AggregationLevelStatus"),
    stringsAsFactors = FALSE
  )
  disorders$active <- startsWith(disorders$status, "Active")
  disorders$definition <- cleanhtml(disorders$definition)

  s <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/SynonymList/Synonym")
  synonyms <- data.frame(
    orphacode = t1(s, "../../OrphaCode"),
    synonym = xml2::xml_text(s),
    stringsAsFactors = FALSE
  )

  a <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/DisorderDisorderAssociationList/DisorderDisorderAssociation")
  redirects <- data.frame(
    orphacode = t1(a, "../../OrphaCode"),
    target = t1(a, "TargetDisorder/OrphaCode"),
    target_name = t1(a, "TargetDisorder/Name"),
    relation = decode(a, "DisorderDisorderAssociationType", orphaids$association),
    stringsAsFactors = FALSE
  )
  # The association is listed on both sides; keep the inactive side only,
  # where the active replacement is the target.
  redirects <- redirects[redirects$relation %in% c("Moved to", "Referred to") &
                           !is.na(redirects$target), , drop = FALSE]

  list(meta = meta, disorders = ascdf(disorders), synonyms = ascdf(synonyms),
       redirects = ascdf(redirects))
}

cleanhtml <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&lt;i&gt;|&lt;/i&gt;", "", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  trimws(x)
}

# Cross references: the pack ICD-10 / ICD-11 files and Orphadata product1
# share the same ExternalReference structure.
parsexrefs <- function(path) {
  doc <- readxml(path)
  meta <- xmlmeta(doc)
  e <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/ExternalReferenceList/ExternalReference")
  rel <- t1(e, "DisorderMappingRelation/Name")
  out <- data.frame(
    orphacode = t1(e, "../../OrphaCode"),
    source = t1(e, "Source"),
    code = t1(e, "Reference"),
    relation = sub("^([A-Z]+) .*$", "\\1", decode(e, "DisorderMappingRelation", orphaids$relation)),
    relation_label = rel,
    icd_relation = decode(e, "DisorderMappingICDRelation", orphaids$icd),
    validation = decode(e, "DisorderMappingValidationStatus", orphaids$validation),
    url = t1(e, "DisorderMappingICDRefUrl"),
    stringsAsFactors = FALSE
  )
  out$source <- normsource(out$source)
  out$url[!is.na(out$url) & !nzchar(out$url)] <- NA_character_
  list(meta = meta, xrefs = ascdf(out))
}

normsource <- function(x) {
  x <- toupper(gsub("[^A-Za-z0-9]", "", x))
  known <- c(ICD10 = "ICD10", ICD11 = "ICD11", OMIM = "OMIM", MONDO = "MONDO",
    UMLS = "UMLS", MESH = "MeSH", MEDDRA = "MedDRA", GARD = "GARD")[x]
  known[is.na(known)] <- x[is.na(known)]
  unname(known)
}

parseclassification <- function(path) {
  doc <- readxml(path)
  meta <- xmlmeta(doc)
  cl <- xml2::xml_find_first(doc, "/JDBOR/ClassificationList/Classification")
  classification <- data.frame(
    classification_id = xml2::xml_attr(cl, "id"),
    classification_code = t1(cl, "OrphaNumber"),
    classification = t1(cl, "Name"),
    root = t1(cl, "ClassificationNodeRootList/ClassificationNode/Disorder/OrphaCode"),
    stringsAsFactors = FALSE
  )
  n <- xml2::xml_find_all(doc, "//ClassificationNode")
  edges <- data.frame(
    classification_id = classification$classification_id,
    parent = t1(n, "../../Disorder/OrphaCode"),
    child = t1(n, "Disorder/OrphaCode"),
    child_name = t1(n, "Disorder/Name"),
    child_type = t1(n, "Disorder/DisorderType/Name"),
    stringsAsFactors = FALSE
  )
  edges <- edges[!is.na(edges$parent), , drop = FALSE]
  edges <- edges[!duplicated(edges[c("parent", "child")]), , drop = FALSE]
  list(meta = meta, classification = classification, edges = ascdf(edges))
}

parselinearisation <- function(path) {
  doc <- readxml(path)
  meta <- xmlmeta(doc)
  a <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/DisorderDisorderAssociationList/DisorderDisorderAssociation[DisorderDisorderAssociationType/@id='21485' or DisorderDisorderAssociationType/Name='Preferential parent']")
  out <- data.frame(
    orphacode = t1(a, "../../OrphaCode"),
    preferential_parent = t1(a, "TargetDisorder/OrphaCode"),
    preferential_parent_name = t1(a, "TargetDisorder/Name"),
    stringsAsFactors = FALSE
  )
  list(meta = meta, linearisation = ascdf(out))
}

parseprevalence <- function(path) {
  doc <- readxml(path)
  meta <- xmlmeta(doc)
  p <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/PrevalenceList/Prevalence")
  out <- data.frame(
    orphacode = t1(p, "../../OrphaCode"),
    type = t1(p, "PrevalenceType/Name"),
    qualification = t1(p, "PrevalenceQualification/Name"),
    class = t1(p, "PrevalenceClass/Name"),
    value = suppressWarnings(as.numeric(t1(p, "ValMoy"))),
    geographic = t1(p, "PrevalenceGeographic/Name"),
    validation = t1(p, "PrevalenceValidationStatus/Name"),
    source = t1(p, "Source"),
    stringsAsFactors = FALSE
  )
  out$class[!is.na(out$class) & !nzchar(out$class)] <- NA_character_
  list(meta = meta, prevalence = ascdf(out))
}

parsegenes <- function(path) {
  doc <- readxml(path)
  meta <- xmlmeta(doc)
  g <- xml2::xml_find_all(doc, "/JDBOR/DisorderList/Disorder/DisorderGeneAssociationList/DisorderGeneAssociation")
  out <- data.frame(
    orphacode = t1(g, "../../OrphaCode"),
    gene_symbol = t1(g, "Gene/Symbol"),
    gene_name = t1(g, "Gene/Name"),
    hgnc = t1(g, "Gene/ExternalReferenceList/ExternalReference[Source='HGNC']/Reference"),
    omim_gene = t1(g, "Gene/ExternalReferenceList/ExternalReference[Source='OMIM']/Reference"),
    ensembl = t1(g, "Gene/ExternalReferenceList/ExternalReference[Source='Ensembl']/Reference"),
    locus = t1(g, "Gene/LocusList/Locus/GeneLocus"),
    association_type = t1(g, "DisorderGeneAssociationType/Name"),
    association_status = t1(g, "DisorderGeneAssociationStatus/Name"),
    source = t1(g, "SourceOfValidation"),
    stringsAsFactors = FALSE
  )
  list(meta = meta, genes = ascdf(out))
}
