
#' @export

cran_deps <- function() {
  resp <- GET(deps_url)
  stop_for_status(resp)
  result <- fromJSON(
    content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  deps <- result$rows
  all_pkgs <- unique(vapply(deps, "[[", "", "key"))

  pkg <- rep(
    vapply(deps, "[[", "", "key"),
    vapply(deps, function(x) length(unlist(x$value)), 1L)
  )

  type <- rep(
    unlist(lapply(deps, function(x) names(x$value))),
    unlist(lapply(deps, function(x) vapply(x$value, length, 1L)))
  )

  dep <- unlist(lapply(deps, function(x) lapply(x$value, names)))

  version <- unlist(lapply(deps, function(x) x$value))

  df <- data.frame(
    stringsAsFactors = FALSE,
    pkg = pkg,
    type = type,
    dep = dep,
    version = version
  )

  row.names(df) <- NULL
  attr(df, "packages") <- all_pkgs
  df
}

hard_dep_types <- c("Depends", "Imports", "LinkingTo")

#' @export
#' @importFrom simplegraph graph topological_sort

cran_topo_sort <- function(deps = NULL) {
  if (is.null(deps)) {
    cat("Downloading dependency data ... ")
    deps <- cran_deps()
    cat("DONE\n")
  }
  all_pkgs <- attr(deps, "packages")

  ## Keep hard dependencies only
  deps <- deps[ deps$type %in% hard_dep_types, ]

  ## Keep CRAN packages only
  deps <- deps[ deps$pkg %in% all_pkgs & deps$dep %in% all_pkgs, ]

  cat("Creating graph ... ")
  edges <- deps[, c("dep", "pkg")]
  verts <- data.frame(
    stringsAsFactors = FALSE,
    id = unique(c(all_pkgs, unlist(edges)))
  )
  sg <- graph(verts, edges)
  cat("DONE\n")

  cat("Topological sorting ... ")
  ts <- topological_sort(sg)
  cat("DONE\n")

  ts
}
