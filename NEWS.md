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
