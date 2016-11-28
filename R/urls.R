
rawdb_url <- "https://crandb.r-pkg.org:6984/cran"

deps_url  <- paste0(rawdb_url, "/_design/app/_view/deps")

rec_deps_url <- "http://crandeps.r-pkg.org/deps"

revdeps_url <- paste0(
  rawdb_url,
  "/_design/app/_list/revdeps/revdeps?limit=10"
)
