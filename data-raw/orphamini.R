## Build the bundled `orphamini` database: a subset of the 2026 Orphanet
## nomenclature restricted to three classifications, rare cardiac diseases
## (146), rare neurological diseases (181) and rare inborn errors of
## metabolism (150), plus a few entities used in examples and tests. The full
## database is obtained with orphafetch("2026") or orphabuild() from the
## RD-CODE files and Orphadata products (CC-BY-4.0).

devtools::load_all()
full_rds <- Sys.getenv("ORPHACODES_FULLDB", "fulldb.rds")
db <- if (file.exists(full_rds)) readRDS(full_rds) else orphafetch("2026", use = FALSE)

e <- db$edges
cls_keep <- c("146", "181", "150")
roots <- db$classifications$root[db$classifications$classification_id %in% cls_keep]
inside <- unique(c(roots, e$child[e$classification_id %in% cls_keep]))
r <- db$redirects
into <- r$orphacode[r$target %in% inside]
keep <- unique(c(inside, into, "166068", "324", "558"))

sub <- function(x, col = "orphacode") x[x[[col]] %in% keep, , drop = FALSE]
d <- sub(db$disorders)
d$preferential_parent_name[!d$preferential_parent %in% keep & !is.na(d$preferential_parent)] <-
  d$preferential_parent_name[!d$preferential_parent %in% keep & !is.na(d$preferential_parent)]
edges <- db$edges[db$edges$classification_id %in% cls_keep & db$edges$parent %in% keep & db$edges$child %in% keep, , drop = FALSE]
cls <- db$classifications[db$classifications$classification_id %in% unique(edges$classification_id), , drop = FALSE]

orphamini <- newdb(
  disorders = d, synonyms = sub(db$synonyms), redirects = sub(db$redirects),
  xrefs = sub(db$xrefs), classifications = cls, edges = edges,
  prevalence = sub(db$prevalence), genes = sub(db$genes),
  meta = modifyList(db$meta, list(subset = "classifications 146 (cardiac), 181 (neurological) and 150 (inborn errors of metabolism)",
                                  sources = NULL, fetched = NULL))
)
print(orphamini)
usethis::use_data(orphamini, overwrite = TRUE, compress = "xz")
