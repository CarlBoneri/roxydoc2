docr.list_ordered <- function(L){
  slots <- unlist(lapply(1:length(L),function(i){
    sprintf("#'   \\item %s", L[[i]])
  }))

  sprintf("#' \\enumerate{\n%s\n#' }", paste0(slots,collapse="\n"))
}
