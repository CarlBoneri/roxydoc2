#' Build an RD formatted output of a dataframe object
#'
#'
#' \code{docr.describe_df}
#'
#'
#'  This is a helper function that works similarly to \code{\link{promptData}},
#'  but instead of returning an .RD formatted output file, this function
#'  \code{\link{cat}}'s a preformatted roxygenized block of information for use
#'  in documenting a data frame. The intention of this function is that when an
#'  alternate or newly created function returns a data.frame object, this function
#'  can be used to easily document the \strong{@@return} code-block.
#'
#'
#' @param df_in The data frame object to be documented
#'
#' @param make_interactive logical Should the columns of the data frame be
#'  additionally documented. If set to \code{\link{TRUE}}, this will create a
#'  prompted loop at the terminal, in which the column name will be printed, and
#'  the provided information can be typed for the column information. The info
#'  is then passed into the item description returned.
#'
#' @return
#' A properly formatted roxygen block describing the data.frame in detail.
#'
#'
#' @family Documentation functions
#'
#'
#' @author Carl S.A. Boneri, \email{carl.boneri@@whyles.com}
#'
#'
#' @examples
#'
#' # A data frame of all local RDS formatted files.
#'
#' a <- dirs.get_r(find_pattern = ".rds$")
#'
#' docr.describe_df(a, make_interactive = T)
#'
#' #' \format{
#' #'   A data frame with 19 observations on the following 5 variables.
#' #'  \describe{
#' #'     \item{\strong{base_path}}{\emph{a character vector}
#' #'         The home path to the library location.}
#' #'     \item{\strong{lib_name}}{\emph{a character vector}
#' #'      The library name of the containing package or
#' #'      directory}
#' #'     \item{\strong{folder_path}}{\emph{a character vector}
#' #'       The sub folder paths, within the directory, in
#' #'        which the file is stored}
#' #'     \item{\strong{file_name}}{\emph{a character vector}
#' #'        The actual, basename of the file itself}
#' #'     \item{\strong{full_path}}{\emph{a character vector}
#' #'         The fully normalized path to the file, for use
#' #'         when sourcing or reading in from an external or
#' #'         alternate library.}
#' #'  }
#' #'}
#'
#' docr.describe_df(a, make_interactive = F)
#'
#' #' \format{
#' #'  A data frame with 19 observations on the following 5 variables.
#' #'  \describe{
#' #'    \item{\strong{base_path}}{\emph{a character vector}}
#' #'    \item{\strong{lib_name}}{\emph{a character vector}}
#' #'    \item{\strong{folder_path}}{\emph{a character vector}}
#' #'    \item{\strong{file_name}}{\emph{a character vector}}
#' #'    \item{\strong{full_path}}{\emph{a character vector}}
#' #'   }
#' #' }
#'
#' @export
#' @importFrom htmltools HTML
docr.describe_df <- function(df_in = NULL, make_interactive = FALSE,
                             include_slice = FALSE){
  paste.html <- function(x){
    HTML(paste0(x, collapse = "\n"))
  }

  df_vars <- list(
    df = df_in,
    n.row = nrow(df_in),
    n.col = ncol(df_in),
    col_names = colnames(df_in)
  )

  # The basic info of the data frame object
  info_top <- sprintf(
    "#'  A data frame with %d observations on the following %d variables.",
    df_vars$n.row, df_vars$n.col
  )

  # The body variables for each vector in the data.frame
  if(make_interactive){

    sub_info <- lapply(df_vars$col_names, function(i){

      spr <- sprintf('Describe:%s', i)
      ans <- ul.ac(readline(spr))

      if(!grepl('\\.$', ans)){
        outs <- sprintf('%s.', ans)
      }else {
        outs <- ans
      }

      C = sprintf("\\emph{%s} formatted vector", class(df_vars$df[[i]]))
      U = sprintf(" with \\emph{%s} unique values: ", length(unique(df_vars$df[[i]])))
      sprintf('#    \\item{\\strong{%s}}{%s}',i, paste0(C, U, outs, collapse = " "))
    })

    sub_items <- lapply(sub_info, function(x){
      paste.html(stri_wrap(x, width = 80,
                           exdent = 6,
                           prefix = "#' ",
                           indent = 3)
      )
    })

    sub_out <- paste.html(sub_items)


  }else {
    sub_out <- lapply(df_vars$col_names, function(i){
      C = sprintf("\\emph{%s} formatted vector", class(df_vars$df[[i]]))
      U = sprintf(" with \\emph{%s} unique values.", length(unique(df_vars$df[[i]])))
      sprintf('#\'    \\item{\\strong{%s}}{%s}',i, paste0(C, U, collapse = " "))
    }) %>% paste.html()
  }

  df_wrap <- paste.html(c("#'  \\describe{", sub_out, "#'   }"))


  whole_wrap <- c("\n#'\n#' \\format{",
                  info_top,
                  df_wrap,
                  "#' }")
  paste.html(whole_wrap)

}
