# Directory Utils ----------------------------------------------------
#
#
#
#' @examples
#' set_scaffold <- list(
#'  inst = list(
#'    www = list('style', 'images', 'lib', 'js'),
#'    "data")
#'  )
#'
#' set_scaffold2 <- list(
#' inst = list(
#'   www = list(
#'     "style",
#'     "js",
#'     "images",
#'     "lib",
#'     "ref_tables",
#'     "templates",
#'     "shiny"
#'   ),
#'
#'   data = list("locker")
#' ),
#' revdep = list("scraps"))
#'
#'
dirs.setup_shell <- function(set_scaffold = NULL){
  library(magrittr)

  dfl_scaffold <- list(
    inst = list(
      www = list(
        "style",
        "js",
        "images",
        "lib",
        "ref_tables",
        "templates",
        "shiny"
      ),

      data = list("locker")
    ),

    revdep = list("scraps"))

  if(!is.null(set_scaffold)){
    dfl_scaffold <- set_scaffold
  }

  # Creates a data.frame of all possible directory names, reverses the melted
  # order which will place the list names first, sub list names second etc..
  # and then removes the completely null columns by finding the colsum of character
  # counts.
  ref_table <- data.table::melt(dfl_scaffold) %>% .[length(.):1] %>%

    dplyr::mutate_all(function(x) {
      ifelse(is.na(x) | grepl('^[0-9]$', x, perl = T), "", x)

    }) %>% .[, colSums(mapply(nchar, .)) > 0]


  # iterates through row by row, extracting only valid cell-values,
  # and creates an iterated list for each sequence ie..
  # list(dir, dir/sub, dir/sub/marine) and unlists, returning only
  # unique value-paths which will be ordered in the naturally created order
  # the system will need.
  all_dirs <- unique(unlist(apply(ref_table, 1, function(i) {
    parts <- as.character(i[nchar(i) > 0])
    sapply(1:length(parts), function(j) {
      stringi::stri_join(parts[seq(1, j)], collapse = "/")
    })
  })))


  # Silently performs a check, and on fail creates each path....
  o <- lapply(all_dirs, function(i) {
    if (!dir.exists(i)) {
      dir.create(i, showWarnings = FALSE)
    }
  })

}
