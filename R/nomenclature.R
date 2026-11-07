#' Look up ORPHAcodes
#'
#' Returns one row per input code, in input order, with the preferred term,
#' status, classification level, typology, aggregation level and
#' preferential parent.
#'
#' @param codes A vector of ORPHAcodes in any accepted format (see
#'   [orphacode()]).
#' @param db An [orphadb]; defaults to the registered database.
#' @param definition Logical; include the Orphanet definition text.
#'
#' @return A data frame with columns `input`, `orphacode`, `found`, `name`,
#'   `status`, `active`, `level`, `type`, `aggregation_code`,
#'   `aggregation_name`, `preferential_parent`, `preferential_parent_name`,
#'   `n_synonyms`, `expert_link` and, if requested, `definition`.
#' @export
#' @examples
#' orphalookup(c("ORPHA:324", 1465, "99999999"), db = orphamini)
orphalookup <- function(codes, db = NULL, definition = FALSE) {
  db <- getdb(db)
  code <- orphacode(codes)
  d <- db$disorders
  i <- match(code, d$orphacode)
  nsyn <- table(db$synonyms$orphacode)
  out <- data.frame(
    input = as.character(codes),
    orphacode = code,
    found = !is.na(i),
    name = d$name[i],
    status = d$status[i],
    active = d$active[i],
    level = d$level[i],
    type = d$type[i],
    aggregation_code = d$aggregation_code[i],
    aggregation_name = d$aggregation_name[i],
    preferential_parent = d$preferential_parent[i],
    preferential_parent_name = d$preferential_parent_name[i],
    n_synonyms = as.integer(ifelse(is.na(code), NA, nsyn[code])),
    expert_link = d$expert_link[i],
    stringsAsFactors = FALSE
  )
  out$n_synonyms[out$found & is.na(out$n_synonyms)] <- 0L
  subsethint(db, !out$found & !is.na(code))
  if (isTRUE(definition)) out$definition <- d$definition[i]
  ascdf(out)
}

#' Search the nomenclature by disease name
#'
#' Case-insensitive search of preferred terms and synonyms. By default the
#' pattern is matched as a substring; set `regex = TRUE` for a regular
#' expression or `fuzzy = TRUE` for approximate matching with
#' [base::agrep()].
#'
#' @param query A single search string.
#' @param db An [orphadb]; defaults to the registered database.
#' @param active_only Logical; drop inactive entities.
#' @param regex,fuzzy Logical; interpret `query` as a regular expression,
#'   or match approximately.
#' @param level Optional classification level filter: any of
#'   `"Group of disorders"`, `"Disorder"`, `"Subtype of disorder"`.
#' @param max Maximum number of entities returned.
#'
#' @return A data frame with `orphacode`, `name`, `matched`, `field`
#'   (`"name"` or `"synonym"`), `status`, `level`, `type`. Entities are
#'   ordered by match quality (preferred-term matches first, then shorter
#'   names).
#' @export
#' @examples
#' orphasearch("fabry", db = orphamini)
#' orphasearch("marfan", db = orphamini, level = "Disorder")
orphasearch <- function(query, db = NULL, active_only = TRUE, regex = FALSE,
                        fuzzy = FALSE, level = NULL, max = 50L) {
  db <- getdb(db)
  if (!is.character(query) || length(query) != 1L) cli::cli_abort("{.arg query} must be a single string.")
  d <- db$disorders
  terms <- rbind(
    data.frame(orphacode = d$orphacode, term = d$name, field = "name", stringsAsFactors = FALSE),
    data.frame(orphacode = db$synonyms$orphacode, term = db$synonyms$synonym, field = "synonym", stringsAsFactors = FALSE)
  )
  hit <- if (isTRUE(fuzzy)) {
    agrepl(query, terms$term, ignore.case = TRUE, max.distance = 0.15)
  } else {
    if (regex) grepl(query, terms$term, ignore.case = TRUE, perl = TRUE)
    else grepl(tolower(query), tolower(terms$term), fixed = TRUE)
  }
  h <- terms[hit, , drop = FALSE]
  h$rank <- (h$field != "name") * 1e6 + nchar(h$term)
  h <- basetable::removeduplicates(basetable::orderrows(h, "rank"), by = "orphacode")
  i <- match(h$orphacode, d$orphacode)
  out <- data.frame(orphacode = h$orphacode, name = d$name[i], matched = h$term,
                    field = h$field, status = d$status[i], level = d$level[i],
                    type = d$type[i], stringsAsFactors = FALSE)
  if (isTRUE(active_only)) out <- out[d$active[i], , drop = FALSE]
  if (!is.null(level)) out <- out[out$level %in% level, , drop = FALSE]
  ascdf(utils::head(out, max))
}

#' Validate ORPHAcodes
#'
#' Checks the format of each code and its status in the release, and
#' proposes the replacement recorded by Orphanet for deprecated codes
#' (`"Moved to"`, always available) and obsolete codes (`"Referred to"`, a
#' suggestion to review).
#'
#' @inheritParams orphalookup
#'
#' @return A data frame with `input`, `orphacode`, `valid` (well formed and
#'   present in the release), `active`, `status`, `issue` (one of `"ok"`,
#'   `"missing"`, `"invalid_format"`, `"unknown"`, `"deprecated"`,
#'   `"obsolete"`, `"non_rare"`), `name`, `replacement`,
#'   `replacement_name` and `relation`.
#' @export
#' @examples
#' orphavalidate(c("ORPHA:324", "166068", "abc", NA, "1"), db = orphamini)
orphavalidate <- function(codes, db = NULL) {
  db <- getdb(db)
  code <- orphacode(codes)
  d <- db$disorders
  i <- match(code, d$orphacode)
  status <- d$status[i]
  issue <- ifelse(is.na(codes) | (is.character(codes) & !nzchar(trimws(as.character(codes)))), "missing",
           ifelse(is.na(code), "invalid_format",
           ifelse(is.na(i), "unknown",
           ifelse(startsWith(status, "Active"), "ok",
           ifelse(grepl("Deprecated", status), "deprecated",
           ifelse(grepl("Non rare", status), "non_rare", "obsolete"))))))
  r <- db$redirects
  j <- match(code, r$orphacode)
  subsethint(db, issue == "unknown")
  ascdf(data.frame(
    input = as.character(codes),
    orphacode = code,
    valid = !is.na(i),
    active = ifelse(is.na(i), FALSE, d$active[i]),
    status = status,
    issue = issue,
    name = d$name[i],
    replacement = r$target[j],
    replacement_name = r$target_name[j],
    relation = r$relation[j],
    stringsAsFactors = FALSE
  ))
}

# Build the matching dictionary (normalised preferred terms + synonyms).
matchdict <- function(db, active_only = TRUE) {
  d <- db$disorders
  if (active_only) d <- d[d$active, , drop = FALSE]
  s <- db$synonyms[db$synonyms$orphacode %in% d$orphacode, , drop = FALSE]
  dict <- rbind(
    data.frame(orphacode = d$orphacode, term = d$name, field = "name", stringsAsFactors = FALSE),
    data.frame(orphacode = s$orphacode, term = s$synonym, field = "synonym", stringsAsFactors = FALSE)
  )
  dict$key <- normtext(dict$term)
  dict <- dict[nzchar(dict$key), , drop = FALSE]
  dict
}

# Words that carry no identity for matching (English and French).
stopwords <- c("syndrome", "disease", "disorder", "type", "of", "the", "and",
               "with", "or", "due", "to", "in", "a", "an", "familial",
               "congenital", "rare", "hereditary", "form", "forme",
               "maladie", "de", "du", "des", "la", "le", "les", "et", "ou",
               "par", "au", "aux", "d", "l", "syndromique", "familiale",
               "congenitale", "hereditaire")

# Sorted informative tokens of a normalised key: makes the comparison
# independent of word order ("syndrome de marfan" ~ "marfan syndrome").
sortkey <- function(key) {
  tok <- strsplit(key, " ", fixed = TRUE)
  vapply(tok, function(t) {
    t <- setdiff(t, stopwords)
    paste(sort(unique(t)), collapse = " ")
  }, character(1))
}

# Fuzzy matching: for each key, the relative edit distance to every
# dictionary entry is the minimum over the raw key and the sorted-token
# key (basetable::textdist, vectorised in C++). Candidates must share at
# least one informative token with the query (or a 4-letter prefix of
# one), and short queries are not fuzzy-matched at all. Returns, per key,
# the index of the best entry, its similarity and the number of distinct
# entities tied at the best distance.
fuzzymatch <- function(keys, dict, max_distance, min_nchar = 5L) {
  if (is.null(dict$skey)) dict$skey <- sortkey(dict$key)
  n1 <- nchar(dict$key); n2 <- nchar(dict$skey)
  toks <- strsplit(dict$skey, " ", fixed = TRUE)
  idx <- rep(seq_along(toks), lengths(toks))
  alltok <- unlist(toks)
  bytok <- split(idx, alltok)
  bypre <- split(idx, substr(alltok, 1, 4))
  res <- lapply(keys, function(k) {
    none <- c(NA_integer_, NA_real_, 0L)
    sk <- sortkey(k)
    if (nchar(k) < min_nchar || !nzchar(sk)) return(none)
    qt <- strsplit(sk, " ", fixed = TRUE)[[1]]
    cand <- unique(c(unlist(bytok[qt], use.names = FALSE),
                     unlist(bypre[substr(qt, 1, 4)], use.names = FALSE)))
    if (!length(cand)) return(none)
    d1 <- basetable::textdist(k, dict$key[cand])[1, ] / pmax(nchar(k), n1[cand])
    d2 <- basetable::textdist(sk, dict$skey[cand])[1, ] / pmax(nchar(sk), n2[cand])
    rel <- pmin(d1, d2)
    ok <- which(rel <= max_distance)
    if (!length(ok)) return(none)
    best <- ok[order(rel[ok], dict$field[cand][ok] != "name", n1[cand][ok])]
    ties <- ok[rel[ok] == rel[best[1]]]
    ncand <- length(unique(dict$orphacode[cand][ties]))
    c(cand[best[1]], 1 - rel[best[1]], ncand)
  })
  do.call(rbind, res)
}

#' Standardise free-text disease names to ORPHAcodes
#'
#' Adds ORPHAcodes to a data frame from a column of disease names. Each
#' name is matched, in order, on the preferred term, on a synonym, and
#' (optionally) approximately. The approximate tier compares names after
#' normalisation (case, accents, punctuation) both as written and as a
#' sorted bag of informative words, so that word order and filler words
#' ("syndrome", "disease", "de", "maladie") do not matter; candidates must
#' share at least one informative word or word stem with the query, and
#' names shorter than `min_nchar` characters are never fuzzy-matched. Ties
#' at the same tier are reported as ambiguous rather than resolved
#' silently.
#'
#' @param data A data frame.
#' @param disease Name of the column holding disease names.
#' @param output Name of the new ORPHAcode column. Three companion columns
#'   are added: `<output>_name`, `<output>_match` and `<output>_score`.
#' @param db An [orphadb]; defaults to the registered database.
#' @param fuzzy Logical; enable approximate matching.
#' @param max_distance Maximum relative edit distance (0 to 1) accepted for a
#'   fuzzy match.
#' @param min_nchar Shortest normalised name eligible for fuzzy matching.
#' @param active_only Logical; match only active entities.
#' @param level Optional classification level restriction, for example
#'   `"Disorder"` to refuse group-level matches.
#'
#' @return `data` with four extra columns. `<output>_match` is one of
#'   `"exact"`, `"synonym"`, `"fuzzy"`, `"ambiguous"` or `"none"`;
#'   `<output>_score` is 1 for exact and synonym matches, the similarity
#'   (1 minus relative distance) for fuzzy ones.
#' @export
#' @examples
#' reg <- data.frame(id = 1:5, dx = c("Fabry disease", "Anderson-Fabry disease",
#'   "Fabri desease", "syndrome de Marfan", "Flu"))
#' orphastandardize(reg, disease = "dx", db = orphamini)
orphastandardize <- function(data, disease, output = "orphacode", db = NULL,
                             fuzzy = TRUE, max_distance = 0.2, min_nchar = 5L,
                             active_only = TRUE, level = NULL) {
  assertdf(data)
  assertcol(data, disease, "disease")
  db <- getdb(db)
  dict <- matchdict(db, active_only = active_only)
  if (!is.null(level)) {
    keep <- db$disorders$orphacode[db$disorders$level %in% level]
    dict <- dict[dict$orphacode %in% keep, , drop = FALSE]
  }
  x <- as.character(data[[disease]])
  key <- normtext(x)
  ukey <- unique(key[!is.na(key) & nzchar(key)])
  res <- data.frame(key = ukey, orphacode = NA_character_, term = NA_character_,
                    match = "none", score = NA_real_, stringsAsFactors = FALSE)
  for (tier in c("name", "synonym")) {
    sub <- dict[dict$field == tier, , drop = FALSE]
    todo <- res$match == "none"
    hits <- lapply(res$key[todo], function(k) unique(sub$orphacode[sub$key == k]))
    n <- lengths(hits)
    one <- which(todo)[n == 1L]
    amb <- which(todo)[n > 1L]
    if (length(one)) {
      res$orphacode[one] <- unlist(hits[n == 1L])
      res$term[one] <- sub$term[match(res$key[one], sub$key)]
      res$match[one] <- if (tier == "name") "exact" else "synonym"
      res$score[one] <- 1
    }
    if (length(amb)) {
      res$match[amb] <- "ambiguous"
      res$term[amb] <- vapply(hits[n > 1L], function(h) paste(h, collapse = ";"), character(1))
    }
  }
  if (isTRUE(fuzzy) && any(res$match == "none")) {
    todo <- which(res$match == "none")
    fm <- fuzzymatch(res$key[todo], dict, max_distance, min_nchar)
    ok <- !is.na(fm[, 1])
    res$orphacode[todo[ok]] <- dict$orphacode[fm[ok, 1]]
    res$term[todo[ok]] <- dict$term[fm[ok, 1]]
    res$score[todo[ok]] <- round(fm[ok, 2], 3)
    res$match[todo[ok]] <- ifelse(fm[ok, 3] > 1, "ambiguous", "fuzzy")
    res$orphacode[todo[ok] [fm[ok, 3] > 1]] <- NA_character_
  }
  i <- match(key, res$key)
  data[[output]] <- res$orphacode[i]
  data[[paste0(output, "_name")]] <- pick(res$orphacode[i], db$disorders, "orphacode", "name")
  data[[paste0(output, "_match")]] <- ifelse(is.na(i), "none", res$match[i])
  data[[paste0(output, "_score")]] <- res$score[i]
  data
}

## ---- hierarchy -----------------------------------------------------------

edgesfor <- function(db, classification = NULL) {
  e <- db$edges
  if (!is.null(classification)) {
    cl <- db$classifications
    id <- cl$classification_id[cl$classification_id %in% classification |
                                 grepl(paste(classification, collapse = "|"), cl$classification, ignore.case = TRUE)]
    e <- e[e$classification_id %in% id, , drop = FALSE]
  }
  e
}

#' Navigate the Orphanet classifications
#'
#' The Orphanet classification is a polyhierarchy: an entity can have several
#' parents and belong to several medical-specialty classifications.
#' `orphaparents()` and `orphachildren()` return direct relations,
#' `orphaancestors()` and `orphadescendants()` the transitive closure with
#' the depth of each relation. `orphagroup()` returns a single roll-up code
#' per entity: the EU aggregation level (closest disorder), the preferential
#' parent (head of the linearised classification), or the root of every
#' classification containing the entity.
#'
#' @param codes ORPHAcodes.
#' @param db An [orphadb]; defaults to the registered database.
#' @param classification Optional classification id (for example `"181"`) or
#'   name fragment (`"neurolog"`) restricting the hierarchy. For
#'   `orphagroup()` it only applies to `level = "classification"`.
#' @param level For `orphagroup()`: `"disorder"` (aggregation level),
#'   `"specialty"` (preferential parent) or `"classification"` (every
#'   classification root, one row per membership).
#'
#' @return A data frame. For the four relation functions: `orphacode`,
#'   `related` (the parent, child, ancestor or descendant), `related_name`,
#'   `related_type`, `classification_id`, `classification` and, for the
#'   transitive versions, `depth`. For `orphagroup()`: `orphacode`, `name`,
#'   `group`, `group_name`, `level`.
#' @export
#' @examples
#' orphaparents("324", db = orphamini)
#' orphachildren("97929", db = orphamini)
#' orphaancestors("324", db = orphamini)
#' orphagroup(c("324", "166068"), db = orphamini, level = "specialty")
orphaparents <- function(codes, db = NULL, classification = NULL) {
  db <- getdb(db)
  e <- edgesfor(db, classification)
  code <- orphacode(codes)
  h <- e[e$child %in% code, , drop = FALSE]
  relationdf(db, h$child, h$parent, h$classification_id)
}

#' @rdname orphaparents
#' @export
orphachildren <- function(codes, db = NULL, classification = NULL) {
  db <- getdb(db)
  e <- edgesfor(db, classification)
  code <- orphacode(codes)
  h <- e[e$parent %in% code, , drop = FALSE]
  relationdf(db, h$parent, h$child, h$classification_id)
}

relationdf <- function(db, from, to, clid, depth = NULL) {
  d <- db$disorders
  cl <- db$classifications
  out <- data.frame(
    orphacode = from, related = to,
    related_name = d$name[match(to, d$orphacode)],
    related_type = d$type[match(to, d$orphacode)],
    classification_id = clid,
    classification = cl$classification[match(clid, cl$classification_id)],
    stringsAsFactors = FALSE
  )
  if (!is.null(depth)) out$depth <- depth
  o <- if (is.null(depth)) order(match(out$orphacode, unique(from)), out$related)
       else order(match(out$orphacode, unique(from)), out$depth, out$related)
  ascdf(out[o, , drop = FALSE])
}

closure <- function(db, codes, classification, up = TRUE) {
  e <- edgesfor(db, classification)
  key <- if (up) e$child else e$parent
  val <- if (up) e$parent else e$child
  res <- list()
  for (c0 in unique(codes)) {
    seen <- character()
    frontier <- c0
    depth <- 0L
    while (length(frontier)) {
      depth <- depth + 1L
      hit <- key %in% frontier
      nxt <- unique(val[hit])
      nxt <- setdiff(nxt, c(seen, c0))
      if (!length(nxt)) break
      res[[length(res) + 1L]] <- data.frame(
        orphacode = c0, related = val[hit][val[hit] %in% nxt],
        classification_id = e$classification_id[hit][val[hit] %in% nxt],
        depth = depth, stringsAsFactors = FALSE)
      seen <- c(seen, nxt)
      frontier <- nxt
    }
  }
  r <- rowbind(res)
  if (is.null(r)) return(relationdf(db, character(), character(), character(), integer()))
  r <- r[!duplicated(r[c("orphacode", "related", "classification_id")]), , drop = FALSE]
  relationdf(db, r$orphacode, r$related, r$classification_id, r$depth)
}

#' @rdname orphaparents
#' @export
orphaancestors <- function(codes, db = NULL, classification = NULL) {
  db <- getdb(db)
  closure(db, orphacode(codes), classification, up = TRUE)
}

#' @rdname orphaparents
#' @export
orphadescendants <- function(codes, db = NULL, classification = NULL) {
  db <- getdb(db)
  closure(db, orphacode(codes), classification, up = FALSE)
}

#' @rdname orphaparents
#' @export
orphagroup <- function(codes, db = NULL, level = c("disorder", "specialty", "classification"),
                       classification = NULL) {
  db <- getdb(db)
  level <- match.arg(level)
  code <- orphacode(codes)
  d <- db$disorders
  i <- match(code, d$orphacode)
  if (level == "disorder") {
    g <- d$aggregation_code[i]
    g[is.na(g) & d$level[i] == "Disorder"] <- code[is.na(g) & d$level[i] == "Disorder"]
    out <- data.frame(orphacode = code, name = d$name[i], group = g,
                      group_name = d$name[match(g, d$orphacode)], level = level,
                      stringsAsFactors = FALSE)
  } else if (level == "specialty") {
    g <- d$preferential_parent[i]
    out <- data.frame(orphacode = code, name = d$name[i], group = g,
                      group_name = d$preferential_parent_name[i], level = level,
                      stringsAsFactors = FALSE)
  } else {
    anc <- orphaancestors(code, db = db, classification = classification)
    cl <- db$classifications
    if (!is.null(classification)) cl <- cl[cl$classification_id %in% unique(edgesfor(db, classification)$classification_id), , drop = FALSE]
    roots <- anc[anc$related %in% cl$root, , drop = FALSE]
    self <- data.frame(orphacode = code[code %in% cl$root], related = code[code %in% cl$root],
                       stringsAsFactors = FALSE)
    g <- rbind(roots[c("orphacode", "related")], self)
    g <- g[!duplicated(g), , drop = FALSE]
    out <- data.frame(orphacode = code, stringsAsFactors = FALSE)
    out <- basetable::merge(out, g, by = "orphacode", all.x = TRUE)
    out <- data.frame(orphacode = out$orphacode, name = d$name[match(out$orphacode, d$orphacode)],
                      group = out$related, group_name = cl$classification[match(out$related, cl$root)],
                      level = level, stringsAsFactors = FALSE)
    out <- out[order(match(out$orphacode, code)), , drop = FALSE]
  }
  ascdf(out)
}
