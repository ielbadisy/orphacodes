
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orphacodes

<!-- badges: start -->

[![R-CMD-check](https://github.com/ielbadisy/orphacodes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ielbadisy/orphacodes/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`orphacodes` makes the Orphanet nomenclature of rare diseases a
versioned, queryable object in R and provides the operations a registry,
a cohort or a surveillance system needs around it:

- **Nomenclature**: download a pinned yearly release (`orphafetch()`),
  look up, search and validate ORPHAcodes, standardise free-text
  diagnoses to codes, navigate the classification.
- **Interoperability**: map to and from ICD-10, ICD-11, OMIM, MONDO,
  UMLS, MeSH, MedDRA and GARD with the relation type and validation
  status preserved.
- **Versioning**: follow the history of a code, update deprecated codes,
  compare two releases.
- **Registry integration**: quality control of a coded dataset,
  harmonisation to one release, choice lists and REDCap dictionaries.
- **Epidemiology**: aggregate cases at the disorder, specialty or
  classification level and look up the prevalence class Orphanet
  publishes for each code.

All tables are returned as `basetable` data frames.

## Installation

``` r
# install.packages("pak")
pak::pak("ielbadisy/orphacodes")
```

## Example

``` r
library(orphacodes)
orphause(orphamini)   # bundled subset; orphafetch("2025") for a full release

orphavalidate(c("ORPHA:324", "166068", "abc"))[, c("input", "issue", "name", "replacement")]
#> # basetable: 3 x 4
#>       input          issue                              name replacement
#> 1 ORPHA:324             ok                     Fabry disease        <NA>
#> 2    166068     deprecated Pontocerebellar hypoplasia type 5      166063
#> 3       abc invalid_format                              <NA>        <NA>
orphastandardize(data.frame(dx = c("Anderson-Fabry disease", "Fabry desease")), disease = "dx")
#>                       dx orphacode orphacode_name orphacode_match
#> 1 Anderson-Fabry disease       324  Fabry disease         synonym
#> 2          Fabry desease       324  Fabry disease           fuzzy
#>   orphacode_score
#> 1           1.000
#> 2           0.923
orphamap("324", to = "ICD10")[, c("orphacode", "code", "relation", "validation")]
#> # basetable: 1 x 4
#>   orphacode  code relation validation
#> 1       324 E75.2     NTBT  Validated
```

A registry extract goes through quality control, harmonisation and
aggregation:

``` r
qc <- orphaqc(orphademo, code = "diagnosis_code", id = "patient_id")
#> ℹ 1 code not in the bundled subset (classifications 146 (cardiac), 181
#>   (neurological) and 150 (inborn errors of metabolism)).
#>   Call `orphafetch()` for the full nomenclature.
#> This message is displayed once per session.
attr(qc, "summary")
#> # basetable: 7 x 3
#>            issue  n  pct
#> 1             ok 89 74.2
#> 2        missing 12 10.0
#> 3     deprecated  8  6.7
#> 4     group_code  6  5.0
#> 5   duplicate_id  2  1.7
#> 6 invalid_format  2  1.7
#> 7        unknown  1  0.8
h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis")
orphaaggregate(h, code = "orphacode", level = "specialty")
#> # basetable: 11 x 3
#>     group                                     group_name  n
#> 1   98006                        Rare neurologic disease 39
#> 2   93890 Rare developmental defect during embryogenesis 32
#> 3   68367                Rare inborn error of metabolism 23
#> 4    <NA>                                           <NA>  8
#> 5  250908                        Rare neoplastic disease  3
#> 6   68416                        Rare infectious disease  3
#> 7   89826                              Rare skin disease  3
#> 8   97978                         Rare endocrine disease  3
#> 9   93419                              Rare bone disease  2
#> 10  97929                           Rare cardiac disease  2
#> # 1 more rows
```

## Reproducibility and review

`orphafetch("2025")` now downloads only the historical nomenclature
pack. Supplementary Orphadata products are distributed as current files.
Mixing these with a historical pack requires an explicit choice:

``` r
db <- orphafetch("2025", include = c("xrefs", "prevalence", "genes"),
                allow_current = TRUE)
db$meta$products  # individual extraction dates, checksums and source URLs
orphasave(db, "analysis-database.rds")  # retain this snapshot with the analysis
```

`orphadiff(old, new)$tables` reports added/removed rows across all
database tables, including mapping metadata, synonyms, definitions and
hierarchy edges. `orphahistory(codes, releases = list(old, new))`
returns history in the supplied release order.

`orphaharmonize()` keeps original codes, replacement paths, matching
tiers and scores. It refuses unresolved inactive codes by default and
marks fuzzy matches and unresolved records with `orphacode_review`.
Branching replacements and cycles require review and are never resolved
by selecting the first target.

QC retains every finding in `.issues`; use `summary(qc, all = TRUE)` to
count all findings, including duplicate identifiers on rows with other
defects. `orphaaggregate(..., id = "patient_id")` counts unique patients
per group and reporting stratum. Without `id`, it counts rows. Patients
in multiple groups may contribute to each group; group totals are not a
unique-patient total.

## Data source

Orphanet nomenclature files for coding (RD-CODE repository) and
Orphadata products, CC-BY-4.0. See `orphacite()`.
