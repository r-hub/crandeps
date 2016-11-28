
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite toJSON fromJSON

pkg_deps <- function(package) {
  assert_that(is.character(package))

  resp <- GET(deps_url, query = list(keys = toJSON(package)))
  stop_for_status(resp)
  result <- fromJSON(
    content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  do.call(
    rbind,
    lapply(result$rows, deps_as_df)
  )
}

deps_as_df <- function(deps) {

  df <- data.frame(
    stringsAsFactors = FALSE,
    pkg = rep(deps$id, length(unlist(deps$value))),
    type = rep(names(deps$value), vapply(deps$value, length, 1L)),
    dep = unlist(lapply(deps$value, function(x) names(x))),
    version = unlist(deps$value)
  )
  row.names(df) <- NULL
  df
}
