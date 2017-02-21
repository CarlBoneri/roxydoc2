#' Recursively extract the symbolic links to all github resources
#'
#'
#' \code{git.recursive}
#'
#'
#' This function takes a either a combination of a username/repo, or a
#'  full path to a rop folder and recurively retreives the file information.
#'
#'
#'
#' @param x The combo of username/repo or full folder path to a github repository.
#' @param filter_pat A pattern to filter the returned data frame for subsetting
#'
#' @return
#'
#' \format{
#'  A data frame with 7 observations on the following 5 variables.
#'  \describe{
#'    \item{\strong{file_name}}{\emph{character}The basename of the file.}
#'    \item{\strong{file_link}}{\emph{character}Full path to the file.}
#'    \item{\strong{file_type}}{\emph{character}File extension or file-type.}
#'    \item{\strong{folder}}{\emph{character} The directory path-folder that contains
#'      the file.}
#'    \item{\strong{raw}}{\emph{character}A full path to the raw github file,
#'      which can be downloaded as if it were a cdn.}
#'   }
#' }
#'
#'
#' @examples
#'
#' had_xml2 <- git.recursive('hadley/xml2')
#' dim(had_xml2)
#'
#' # get the first R file from the Ropensci Magick repo
#' git.recursive('ropensci/magick', filter_pat = "\\.R$") %>%
#'  slice(1) %>% .$raw
#'
#'
#' @export

git.recursive <- function(x, filter_pat = NULL){
  ul.ac <- function(x){
    x %>% unlist %>% as.character()
  }
  is.file <- function(x){
    nchar(file_ext(x)) > 0
  }

  fi.get_ext <- function(x){
    ul.ac(stri_extract_all_regex(x, "\\.([[:alnum:]]+)$", simplify = T))
  }

  fi.get_name <- function(x){
    if(exists('basename')){
      base_name <- function(x){
        ul.ac(lapply(stri_split_regex(x, "/"), function(i)i[length(i)]))
      }
      base_name(x)
    }else{
      basename(x)
    }
  }


  git_dir <- '.css-truncate-target .js-navigation-open'

  m.pth <- function(x){
    if(!grepl('^http', x)){
      pth <- sprintf('https://github.com%s',
                     ifelse(!grepl('^/', x),
                            paste0("/", x), x))
    }else{
      pth <- x
    }
    return(pth)
  }

  r.pth <- function(x){
    gsub('https://github.com',
         'https://raw.githubusercontent.com' ,
         gsub('/blob','',x))
  }

  sets <- html_nodes(html(m.pth(x)), git_dir)

  ref_idx <- lapply(sets, function(i){
    data.frame(file_link = m.pth(html_attr(i, "href")),
               name = html_attr(i, "title")) %>%
      mutate(type_of = ifelse(is.file(name), 'file', 'dir')) %>%
      mutate(file_type = fi.get_ext(name),
             file_name = fi.get_name(name))
  }) %>% rbind.pages()

  ref_idx$folder <- ul.ac(
    llply(1:nrow(ref_idx), function(i){
      stri_replace_all_regex(
        ref_idx[i,'file_link'],
        sprintf("%s|/%s",
                "https://github.com/(\\w+)/(\\w+)/blob/master/",
                ref_idx[i, "file_name"]),
        "")
    })
  )


  ref_idx$raw <- ul.ac(llply(ref_idx$file_link, r.pth))

  if(any(ref_idx$type_of == 'dir')){
    old <- ref_idx %>% dplyr::filter(type_of == "file")
    new <- lapply(ref_idx %>% dplyr::filter(type_of == "dir") %>% .$file_link,
                  git.recursive) %>% rbind.pages()
    out <- rbind(old, new)

  }else{
    out <- ref_idx
  }

  if(!is.null(filter_pat)){
    out <- out[grepl(filter_pat, out$name),]
  }

  return(out)
  # out %>% select(file_name, file_link = next_link, file_type, folder, raw)
}

f.git <- git.recursive
