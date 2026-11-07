
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
  classification level and report observed prevalence beside the
  Orphanet reference class.

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
orphastandardize(data.frame(dx = c("Anderson-Fabry disease", "Fabri desease")), disease = "dx")
#>                       dx orphacode orphacode_name orphacode_match
#> 1 Anderson-Fabry disease       324  Fabry disease         synonym
#> 2          Fabri desease       324  Fabry disease           fuzzy
#>   orphacode_score
#> 1           1.000
#> 2           0.846
orphamap("324", to = "ICD10")[, c("orphacode", "code", "relation", "validation")]
#> # basetable: 1 x 4
#>   orphacode  code relation validation
#> 1       324 E75.2     NTBT  Validated
```

A registry extract goes through quality control, harmonisation and
aggregation:

``` r
qc <- orphaqc(orphademo, code = "diagnosis_code", id = "patient_id")
attr(qc, "summary")
#> # basetable: 7 x 3
#>            issue  n  pct
#> 1             ok 90 75.0
#> 2        missing 12 10.0
#> 3     deprecated  8  6.7
#> 4     group_code  6  5.0
#> 5 invalid_format  2  1.7
#> 6   duplicate_id  1  0.8
#> 7        unknown  1  0.8
h <- orphaharmonize(orphademo, code = "diagnosis_code", disease = "diagnosis")
orphaaggregate(h, code = "orphacode", level = "specialty")
#> # basetable: 13 x 3
#>     group                                     group_name  n
#> 1   68367                Rare inborn error of metabolism 47
#> 2   98006                        Rare neurologic disease 25
#> 3   97929                           Rare cardiac disease 17
#> 4    <NA>                                           <NA>  8
#> 5   93890 Rare developmental defect during embryogenesis  7
#> 6  250908                        Rare neoplastic disease  3
#> 7   98004                            Rare immune disease  3
#> 8   98023         Rare systemic or rheumatologic disease  3
#> 9   97992                       Rare hematologic disease  2
#> 10  98036              Rare otorhinolaryngologic disease  2
#> # 3 more rows
```

## Data source

Orphanet nomenclature files for coding (RD-CODE repository) and
Orphadata products, CC-BY-4.0. See `orphacite()`.
