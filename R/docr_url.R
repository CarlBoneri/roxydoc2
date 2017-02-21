#' Create a formatted url and automatically append the page title if possible
#'
#'
#' \code{docr.url}
#'
#'
#'
#' @export
docr.url <- function(url){
  if(identical(attr(curlGetHeaders(url), 'status'), 200L)){

    out <- sprintf('\\href{%s}{%s}', url,
                   rvest::html_text(
                     rvest::html_nodes(
                       rvest::html(url),"title")))
  }else {
    out <- sprintf('\\url{%s}', url)
  }
  htmltools::HTML(out)
}
