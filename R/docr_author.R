#' Build an RD formatted Author line
#'
#'
#' \code{docr.author}
#'
#' Set the author line for any documentation or R scripts.
#'
#' @return The formatted roxygen line for the author.
#'
#'
#' @family Documentation functions
#'
#' @author Carl S.A. Boneri, \email{carl.boneri@@whyles.com}
#'
#' @examples
#' me <- list(name = "Carl S.A. Boneri", email = "carl.boneri@whyles.com")
#' docr.author(me$name, me$email)
#'
#' # The function handles improperly specified email addresses containing only
#' # one \code{'@'} symbol.
#'
#' #' @author Carl S.A. Boneri, \email{carl.boneri@@whyles.com}
#'
#'
#' @export
#' @importFrom htmltools HTML
docr.author <- function(full_name = NULL, email = NULL){
  if(!is.null(docr.get_defaults()) & is.null(full_name)){
    auth_vars <- as.list(docr.get_defaults()[,c('author','email')])
  }else{
    auth_vars <- list(author = full_name, email = email)
  }

  set_email <- ifelse(grepl('@@', auth_vars$email),
                      auth_vars$email,
                      gsub("@", "@@", auth_vars$email))

  cat(sprintf("#' @author %s, \\email{%s}",auth_vars$author,set_email))

}
