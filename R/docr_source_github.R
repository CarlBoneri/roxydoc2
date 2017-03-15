#' Build only R files for the docr package from github source repo
#'
#' \code{\link{docr.install_r_only}}
#'
#' @param output_dir The directory to store the files to.
#'
#' @param source_on_load logical After the files have been moved into the local 
#'  directory, should they be sourced into the global environment. Defaults to
#'  true.
#' 
#' 
#' @family Documentation functions
#'
#' @author Carl S.A. Boneri, \email{carl@@gmail.com}
#'
#' @return The structured function.
#'
#'
#' @examples
#'
#' docr.install_r_only("revdep")
#'
#' @export


docr.install_r_only <- function(output_dir = "R", source_on_load = TRUE){
  
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  o <- suppressMessages(suppressWarnings(
    git.recursive("CarlBoneri/roxydoc2", filter_pat = "R$") %>% .$raw %>% 
      lapply(., function(i){
        out_path <- sprintf("%s/%s",output_dir,basename(i))
        download.file(i, out_path, quiet = TRUE)
        if(source_on_load && file.exists(out_path)){
          base::source(out_path, echo = FALSE, print.eval = FALSE)
        }
      })
  ))
}
