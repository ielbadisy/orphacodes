## Build the bundled `orphamini` database: a subset of the 2026 Orphanet
## nomenclature restricted to the rare cardiac diseases classification
## (id 146) plus a few entities used in examples and tests. The full
## database is obtained with orphafetch("2026") or orphabuild() from the
## RD-CODE files and Orphadata products (CC-BY-4.0).

devtools::load_all()
full_rds <- Sys.getenv("ORPHACODES_FULLDB", "fulldb.rds")
db <- if (file.exists(full_rds)) readRDS(full_rds) else orphafetch("2026", use = FALSE)

e <- db$edges
cardiac <- unique(c("97929", e$child[e$classification_id == "146"]))
r <- db$redirects
into <- r$orphacode[r$target %in% cardiac]
neuro <- c("98006", e$child[e$classification_id == "181" & e$parent == "98006"])
pch5 <- c("166068", r$target[r$orphacode == "166068"])
anc <- orphaancestors(pch5[2], db = db, classification = "181")$related
keep <- unique(c(cardiac, into, neuro, pch5, anc, "324", "558"))

sub <- function(x, col = "orphacode") x[x[[col]] %in% keep, , drop = FALSE]
d <- sub(db$disorders)
d$preferential_parent_name[!d$preferential_parent %in% keep & !is.na(d$preferential_parent)] <-
  d$preferential_parent_name[!d$preferential_parent %in% keep & !is.na(d$preferential_parent)]
edges <- db$edges[db$edges$parent %in% keep & db$edges$child %in% keep, , drop = FALSE]
cls <- db$classifications[db$classifications$classification_id %in% unique(edges$classification_id), , drop = FALSE]

orphamini <- newdb(
  disorders = d, synonyms = sub(db$synonyms), redirects = sub(db$redirects),
  xrefs = sub(db$xrefs), classifications = cls, edges = edges,
  prevalence = sub(db$prevalence), genes = sub(db$genes),
  meta = modifyList(db$meta, list(subset = "rare cardiac diseases (classification 146) and examples",
                                  sources = NULL, fetched = NULL))
)
print(orphamini)
usethis::use_data(orphamini, overwrite = TRUE, compress = "xz")
