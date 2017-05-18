#' Function helpers
#'
#'

grab_args <- function() {
  envir <- parent.frame()
  func <- sys.function(-1)
  call <- sys.call(-1)
  dots <- match.call(func, call, expand.dots=FALSE)$...
  c(as.list(envir), dots)
}
