
#' @export

pkg_recursive_deps <- function(package, soft = TRUE) {
  assert_that(is_string(package))
  assert_that(is_flag(soft))

  url <- paste0(rec_deps_url, "/", if (soft) "all/", package)
  resp <- GET(url)
  stop_for_status(resp)
  deps <- fromJSON(
    content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  ## drop dummy dependencies of base packages
  deps <- deps[ ! vapply(deps, identical, FALSE, FALSE) ]

  df <- data.frame(
    stringsAsFactors = FALSE,
    pkg = rep(names(deps), vapply(deps, function(x) length(unlist(x)), 1L)),
    type = unlist(lapply(deps, function(x) rep(names(x), vapply(x,
      length, 1L)))),
    dep = unlist(lapply(deps, function(x) lapply(x, names))),
    version = unlist(deps)
  )

  ## Remove version numbers from versions strings, for now
  df$pkg <- sub("-.*$", "", df$pkg)
  df$dep <- sub("-.*$", "", df$dep)

  ## Remove line breaks from versions, these should not be here ideally
  df$version <- gsub("\n", " ", df$version)

  row.names(df) <- NULL
  df
}
