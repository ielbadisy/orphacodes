`%||%` <- function(x, y) if (is.null(x)) y else x

#' Normalise ORPHAcode identifiers
#'
#' Accepts the many ways an ORPHAcode is written in practice
#' (`"ORPHA:324"`, `"orpha324"`, `"ORPHA 324"`, `324`, `" 324 "`) and
#' returns the canonical bare code as a character string (`"324"`). Values
#' that do not contain a valid numeric code become `NA`.
#'
#' @param x A character or numeric vector of candidate ORPHAcodes.
#' @param prefix Logical; return the codes with the `"ORPHA:"` prefix
#'   (`TRUE`) or as bare digits (`FALSE`, the default used everywhere in the
#'   package).
#'
#' @return A character vector of the same length as `x`.
#' @export
#' @examples
#' orphacode(c("ORPHA:324", "orpha 324", 324, "  324 ", "Q78.0", NA))
#' orphacode("324", prefix = TRUE)
orphacode <- function(x, prefix = FALSE) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    out <- ifelse(is.na(x) | x != floor(x) | x < 0, NA_character_,
                  format(x, scientific = FALSE, trim = TRUE))
  } else {
    s <- trimws(as.character(x))
    s <- sub("^(?i)orpha(net)?[[:space:]:_-]*", "", s, perl = TRUE)
    s <- sub("^0+(?=[0-9])", "", s, perl = TRUE)
    out <- ifelse(grepl("^[0-9]{1,9}$", s), s, NA_character_)
  }
  if (isTRUE(prefix)) out <- ifelse(is.na(out), out, paste0("ORPHA:", out))
  out
}

# Normalise a disease name for matching: lower case, ASCII transliteration,
# possessives dropped ("Huntington's" -> "huntington"), British spellings
# folded to the American ones used by Orphanet (leukaemia, haemophilia,
# tumour, oesophageal), punctuation to spaces, collapsed whitespace.
normtext <- function(x) {
  x <- gsub("['\u2019\u0092`]s\\b", "", as.character(x), perl = TRUE)
  x <- basetable::lower(basetable::transliterate(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  for (i in seq_along(spelling)) x <- gsub(names(spelling)[i], spelling[[i]], x, perl = TRUE)
  basetable::squish(x)
}

spelling <- c("aem" = "em", "\\boes" = "es", "\\boe(?=d)" = "e", "our\\b" = "or",
              "\\bpaed" = "ped", "\\bfoet" = "fet", "\\borthopaed" = "orthoped")

assertdf <- function(x, arg = "data") {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data frame, not {.cls {class(x)}}.")
  }
  invisible(x)
}

assertcol <- function(data, col, arg) {
  if (!is.character(col) || length(col) != 1L) {
    cli::cli_abort("{.arg {arg}} must be a single column name.")
  }
  if (!col %in% names(data)) {
    cli::cli_abort("Column {.val {col}} not found in {.arg data}.")
  }
  invisible(col)
}

emptydf <- function(...) {
  cols <- list(...)
  as.data.frame(lapply(cols, function(type) vector(type, 0L)),
                stringsAsFactors = FALSE)
}

# Row-bind a list of data frames (NULL elements dropped) with basetable.
rowbind <- function(lst) {
  lst <- lst[!vapply(lst, is.null, logical(1))]
  if (!length(lst)) return(NULL)
  ascdf(do.call(basetable::rbindfill, unname(lst)))
}

# Lookup helper: first match of `key` in `table[[by]]`, returning
# `table[[value]]`.
pick <- function(key, table, by, value) {
  table[[value]][match(key, table[[by]])]
}

# Every table returned by the package is a basetable (a data.frame with
# the "basetable" class, printed compactly).
ascdf <- function(df) {
  rownames(df) <- NULL
  basetable::asbasetable(df)
}
