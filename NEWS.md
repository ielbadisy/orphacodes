# orphacodes (development version)

* `orphaprevalence()` is removed. Rates and confidence intervals are
  general epidemiology, outside the scope of an Orphanet toolkit; count
  cases with `orphaaggregate()`, compute the rate with your own
  denominator, and compare with `orphaprevref()`.
* Fuzzy matching in `orphastandardize()` counts a swap of two adjacent
  letters as one edit instead of two, so common typing errors such as
  "Cysitc fibrosis" are matched under the 0.1 default.
* `orphastandardize()` (and `orphaharmonize()` through it) now accepts a
  fuzzy match only at a similarity of 0.9 or more (`max_distance = 0.1`,
  was 0.2). Looser matches were mostly another disease, such as
  non-small cell lung cancer coded as small cell lung cancer. Pass
  `max_distance = 0.2` to get the old behaviour.
* `orphastandardize()` no longer ignores qualifiers such as "familial",
  "hereditary", "congenital", "rare" or a single letter when matching, so
  "pancreatic cancer" is no longer coded as familial pancreatic carcinoma
  and "hemophilia" is not taken for hemophilia A. British spellings
  (leukaemia, haemophilia, tumour, oesophageal, paediatric) and
  possessives ("Huntington's disease") are normalised before matching and
  now match exactly.
* Downloads use a 15-minute timeout instead of R's 60-second default, so
  the large Orphadata products (cross-references, prevalence) no longer
  fail on a slow connection. A failed English Orphadata product now
  raises a warning instead of leaving an empty table silently.

# orphacodes 1.0.1

* `orphafetch()` rebuilds a release from the cached pack files without
  calling the GitHub API, so switching products (`include`,
  `classifications`) or re-running a pipeline no longer counts against
  the 60 requests per hour quota. A manifest of downloaded files is kept
  in the cache directory.
* The 2019 and 2020 nomenclature packs, which use the older element
  names (`OrphaNumber`, `GroupOfType`, `OutDisorder`, `InDisorder`), are
  parsed to the same tables as later releases.
* Absent pack directories (no ICD-11 alignment or linearisation before
  2021) no longer emit raw 404 warnings.

# orphacodes 1.0.0

* Language-independent parsing: status, classification level, entity
  type, redirect relations, preferential parents, mapping relations, ICD
  relations and validation status are decoded from the Orphanet
  identifiers and FlagValue, so a French (or any other) release gives
  the same canonical labels and the same behaviour as the English one.
* Fuzzy matching rewritten: candidates share an informative token or
  prefix, French and English filler words are ignored, word order is
  ignored through sorted-token keys, short strings (`min_nchar`) are
  never fuzzy-matched and ties are reported as ambiguous.
* `orphaprevalence()` gains stratified estimates (`by` with a population
  table), directly standardised rates (`standard` weights, Fay-Feuer
  gamma interval), zero-count strata and a `method` column.
* `orphamini` now covers three classifications (cardiac, neurological,
  inborn errors of metabolism; 4,598 entities) and functions mention
  once when codes fall outside the bundled subset.
* `orphagroup()` gains a `classification` argument.
* `orphadiff()`: the `renamed` table no longer duplicates the name
  column; verified on the real 2024 to 2025 releases.
* `orphademo` resampled towards shorter, common names.
* Vignette: real release diff, stratified and standardised prevalence,
  under-ascertainment, other languages.

# orphacodes 0.1.0

* First release.
* Nomenclature: `orphafetch()` (yearly releases pinned through the
  RD-CODE repository), `orphabuild()`, `orphalookup()`, `orphasearch()`,
  `orphavalidate()`, `orphastandardize()`, hierarchy navigation
  (`orphaparents()`, `orphachildren()`, `orphaancestors()`,
  `orphadescendants()`, `orphagroup()`).
* Interoperability: `orphamap()`, `orphafrom()`, `orphasources()` with
  relation types and validation status preserved.
* Versioning: `orphahistory()`, `orphaupdate()`, `orphadiff()`.
* Registry integration: `orphaqc()`, `orphaharmonize()`,
  `orphachoices()`, `orpharedcap()`.
* Epidemiology: `orphaaggregate()`, `orphaprevalence()`,
  `orphaprevref()`.
* Bundled data: `orphamini` (rare cardiac diseases subset of the 2026
  release) and `orphademo` (simulated registry).
