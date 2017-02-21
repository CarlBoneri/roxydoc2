#' Text formatting helpers for RD documentation
#'
#'
#' \code{docr.txt}
#'
#' @param format The formatting variable for the text object; one of:
#'
#'  \describe{
#'    \item{italics}{emphasises text in italics}
#'    \item{strong}{creates bold text}
#'    \item{code}{formats inline code}
#'    \item{preformatted}{format ext as-is, useful for multi-line code}
#'    \item{url}{a url or web reference}
#'    \item{href}{url with custom link}
#'    \item{email}{an email address}
#'  }
#'
#' @param ... The text body
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
#'
#' @export
docr.txt <- function(type = c('italics','strong','code','preformatted',
                              'url','href','email'),
                     ...){

  t.type <- match.arg(type)

  switch(t.type,
         "italics" = sprintf("\\emph{%s}",...),
         "strong" = sprintf("\\strong{%s}",...),
         "code" = sprintf("\\code{%s}",...),
         "preformatted" = sprintf("\\preformatted{%s}",...),
         "url" = sprintf("\\url{%s}",...),
         "href" = sprintf("\\href{%s}",...),
         "email" = sprintf("\\email{%s}",...)
  )
}
