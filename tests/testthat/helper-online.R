# Skip online tests when the unauthenticated GitHub API quota is exhausted.
skip_if_ratelimited <- function() {
  if (nzchar(Sys.getenv("GITHUB_PAT"))) return(invisible(TRUE))
  left <- tryCatch({
    con <- url("https://api.github.com/rate_limit")
    on.exit(close(con))
    txt <- paste(readLines(con, warn = FALSE), collapse = "")
    txt <- gsub("[[:space:]]", "", txt)
    core <- regmatches(txt, regexpr('"core":\\{[^}]*\\}', txt))
    as.integer(sub('.*"remaining":([0-9]+).*', "\\1", core))
  }, error = function(e) NA_integer_)
  if (!is.na(left) && left < 10) testthat::skip("GitHub API rate limit reached")
  invisible(TRUE)
}
