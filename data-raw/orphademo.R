## Simulated rare-disease registry extract used in examples and tests.
## Codes and names come from `orphamini`; the data mimic the defects met
## in real registries: free-text names with synonyms and typos, codes with
## prefixes, deprecated codes, group-level codes, missing values and a
## duplicated patient identifier.

devtools::load_all()
set.seed(20261106)
d <- orphamini$disorders
act <- d[d$active & d$level == "Disorder" & d$orphacode %in% orphamini$edges$child, ]
n <- 120
# favour short, common-looking names, as real registries do
pick_codes <- sample(act$orphacode, n, replace = TRUE, prob = 1 / rank(nchar(act$name))^0.5)
code <- pick_codes
name <- d$name[match(code, d$orphacode)]

# synonyms for some names
syn <- orphamini$synonyms
i <- which(code %in% syn$orphacode)[1:25]
name[i] <- vapply(code[i], function(c0) sample(syn$synonym[syn$orphacode == c0], 1), character(1))
# typos
typo <- function(x) { p <- sample(nchar(x) - 1, 1); paste0(substr(x, 1, p - 1), substr(x, p + 1, nchar(x))) }
j <- sample(setdiff(seq_len(n), i), 15); name[j] <- vapply(name[j], typo, character(1))
# prefixes and formats
fmt <- sample(c("%s", "ORPHA:%s", "orpha %s", "ORPHA%s"), n, TRUE, prob = c(.7, .15, .1, .05))
code_txt <- sprintf(fmt, code)
# deprecated codes (Moved to) pointing into the subset
r <- orphamini$redirects[orphamini$redirects$relation == "Moved to", ]
k <- sample(seq_len(n), 8); code_txt[k] <- sample(r$orphacode, 8, replace = TRUE)
name[k] <- d$name[match(code_txt[k], d$orphacode)]
# group codes, missing codes, junk
g <- d$orphacode[d$level == "Group of disorders" & d$active]
l <- sample(setdiff(seq_len(n), k), 6); code_txt[l] <- sample(g, 6); name[l] <- d$name[match(code_txt[l], d$orphacode)]
m <- sample(setdiff(seq_len(n), c(k, l)), 12); code_txt[m] <- NA
code_txt[sample(setdiff(seq_len(n), c(k, l, m)), 3)] <- c("Q87.4", "abc", "99999999")
name[sample(m, 2)] <- c("Influenza", "Unknown cardiomyopathy")

orphademo <- data.frame(
  patient_id = sprintf("P%04d", c(1:(n - 1), 17)),
  sex = sample(c("F", "M"), n, TRUE),
  birth_year = sample(1950:2020, n, TRUE),
  diagnosis = name,
  diagnosis_code = code_txt,
  stringsAsFactors = FALSE
)
orphademo <- orphademo[sample(n), ]
rownames(orphademo) <- NULL
usethis::use_data(orphademo, overwrite = TRUE)
