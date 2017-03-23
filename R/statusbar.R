
#' @importFrom statusbar status_bar status_log
#' @importFrom crayon bold blue green
#' @importFrom clisymbols symbol

with_status <- function(expr, msg, done = "DONE", width = 35) {  

  title <- blue $ bold
  current <- bold

  msg35 <- dotted_line(msg, width = 35)
  status_bar(current(" ", msg35))

  on.exit({
    status_bar(NULL)
    status_log(paste(green(symbol$tick), green(msg35, done)))
  })
  expr
}

#' @import backports

dotted_line <- function(msg, width) {
  paste(msg, strrep(".", max(0, width - nchar(msg))))
}
