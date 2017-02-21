#' Properly wrap free-hand RD text
#'
#' \code{docr.wrap}
#'
#' This is text
#'  t
#'
#' @examples
#' # Created a line of body text with formatting
#' my_text <- paste0(collapse=" ",
#'              c('A function which allows passing',
#'                docr.txt('italics','external'),
#'                'arguments in the form of',
#'                docr.txt('code','alist')))
#'
#' > docr.wrap(my_text)
#' # #'  A function which allows passing \\emph{external} arguments
#' # #'   in the form of \\code{alist}
#'
#' @export

docr.wrap <- function(x = NULL, gap_btw_lines = TRUE, pre_fix = "#'",
                      file = NULL, assign_as = NULL){
  # Ensure set encoding and remove ASCII escape characters that won't appear
  # in the raw string, but will translate literally to the destination
  # object, which will royally fuck trying to source R scripts in a function.
  doc_in <- enc2utf8(x) %>% stri_replace_all_regex('(\032){1,}', '')

  if(length(doc_in) == 1){
    doc_in <- stri_wrap(doc_in, 80)
  }else {
    doc_in <- doc_in
  }

  # If the desired
  if(gap_btw_lines){

    doc_in <- unlist(lapply(doc_in, function(i){
      if(!stri_detect_regex(i, "^(\\#){1,}+")){
        c(i, pre_fix)
      }else{
        i
      }
    })) %>% (function(x){

      if(any(grepl('\\#$', x))){
      f_end <- max(grep("\\#$",x))
      c(x[1:(f_end)], pre_fix, x[(f_end + 1):(length(x))])
      }else {
        x
      }
    })

#
#     x <- c(pre_fix, unlist(
#       sapply(x, function(i){
#         c(i, pre_fix)
#         }, USE.NAMES = F, simplify = F)
#       ))

  }

  doc_pred <- unlist(lapply(doc_in, function(i){
    if(!grepl('^\\#', i)){
      sprintf("%s %s",pre_fix, i)
    }else {
      i
    }
  }))


  if(!all(stri_width(stri_wrap(x, 80)) <= 80)){
    set_width <- 80
  }else {
    set_width <- floor(0.9 * getOption("width"))
  }

  set_lines <- stri_wrap(doc_pred,
                         width = set_width,
                         simplify = TRUE,
                         cost_exponent = 0.0,
                         whitespace_only = TRUE,
                         use_length = TRUE,
                         normalize = TRUE,
                         indent = 0,
                         exdent = 2)

#
#     set_lines <- stri_replace_all_regex(
#       set_lines, sprintf('^%s %s$', pre_fix, pre_fix), pre_fix)


    return(set_lines)

    if(!is.null(file)){

      if(!nchar(file_ext(file))){
        file <- sprintf('%s.txt', file)
    }

    if(!file.exists(file)){
      file.create(file)
    }

    writeLines(set_lines, sep = "\n", con = file)

  }else if(!is.null(assign_as)){
    assign(assign_as, set_lines, envir = .GlobalEnv)
  }else {
      cat(set_lines, sep = "\n")
  }

}
