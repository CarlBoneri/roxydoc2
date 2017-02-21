#' Create an RD list from an R list
#'
#' \code{docr.list}
#'
#'
#' @param type Type of either:
#' \describe{
#'  \item{ordered}{A numbered list}
#'  \item{unordered(bulleted)}{A bullet list}
#'  \item{named}{A pair list of named objects}
#'}
#'
#' @param L The list; if pre-constructed
#'
#'
#' @examples
#' A <- list(
#'        Linux = 'server or machine type running in a UNIX/Linux environment',
#'        Windows = 'Running win-32 or win64 environment',
#'        `Mac OS` = 'Running MACOS system type'
#'      )
#'
#'  # Since all list variables are named, the function defaults to `named`
#'  # list-type
#'  > docr.list(L = A)
#'	##' \\describe{
#'	##'   \\item{Linux}{server or machine type running in a UNIX/Linux environment}
#'	##'   \\item{Windows}{Running win-32 or win64 environment}
#'	##'   \\item{Mac OS}{Running MACOS system type}
#'	##' }
#'
#'	> docr.list(L = A, type = "unordered")
#'	##' \\itemize{
#'	##'   \\item server or machine type running in a UNIX/Linux environment
#'	##'   \\item Running win-32 or win64 environment
#'	##'   \\item Running MACOS system type
#'	##' }
#'
#'	> docr.list(L = A, type = "ordered")
#'	##' \enumerate{
#'	##'   \item server or machine type running in a UNIX/Linux environment
#'	##'   \item Running win-32 or win64 environment
#'	##'   \item Running MACOS system type
#'	##' }
#'
#' @export
docr.list <- function(type = c('ordered', 'unordered', 'named'), L){
  all.named <- function(x){
    length(names(x)) == length(x)
  }

  if(length(type) == 3){

    if(all.named(L)){
      docd_list <- docr.list_named(L)
    }else if(length(L) >= 5){
      docd_list <- docr.list_ordered(L)
    }else {
      docd_list <- docr.list_bullet(L)
    }
  }else {
    l.type <- match.arg(type)

    docd_list <- switch(l.type,
                        'ordered' = docr.list_ordered(L),
                        'unordered' = docr.list_bullet(L),
                        'named' = docr.list_named(L)
    )
  }

  return(docd_list)
}
