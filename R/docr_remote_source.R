#' Arg helper
#'
#' \code{grab_args}
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


#' HTTP/HTTPS check
url.ok <- function(x, ...){
  identical(status_code(HEAD(x, ...)), 200L)
}

#' DOCR source files from github repo
#'
#' \code{docr.remote_source}
#'
#'
#'
#' @examples
#' docr.remote_source(author = "Carl Boneri", email = "carl.boneri@whyles.com",
#'                    company = "Whyles Inc", meta = NULL,
#'                    website = "https://whyles.com", github = "CarlBoneri")
#'
#'
docr.remote_source <- function(author = NULL, email = NULL, company = NULL,
                               meta = NULL, website = NULL, github = NULL){

  user_vargs <- grab_args()

  git_repo <- 'CarlBoneri/roxydoc2'

  git_base <- 'https://raw.githubusercontent.com/CarlBoneri/roxydoc2/master/R/'

  recursive_file <- 'github_recursive.R'

  dox_git <- paste0(git_base, recursive_file)



  if(httr::url_ok(dox_git)){
    # Source recursive web parser
    suppressMessages(
      devtools::source_url(dox_git)
    )
  }

  all_df <- git.recursive(git_repo, filter_pat = "(.*?)\\.R$")

  all_paths <- all_df[['raw']]

  dox_source_files <- grep('source|recursive', all_paths,
                           perl = TRUE, value = TRUE,
                           invert = TRUE)

  if(all(unlist(parallel::mclapply(dox_source_files, url.ok)))){
    o <- lapply(dox_source_files, function(i){
      suppressMessages(
        devtools::source_url(i)
      )
    })

    if(any(!is.null(user_vargs)) && exists('docr.defaults')){
      docr.defaults(author = author, email = email,
                    company = company, meta = meta, website = website,
                    github = github)
    }
  }

}
