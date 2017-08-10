#' Create a string of #
#'
#' \code{pound}
#'
#'
#' @examples
#'
#' > pound(n = 14)
#' [1] "##############"
#' > pound(n = 14, "--")
#' [1] "----------------------------"
pound <- function(n = NULL, use_symbol = "#"){
  parts <- rep(use_symbol, n)
  stringi::stri_join(parts, collapse = "")
}


#' Check if an object variable is character encoded
vargs.is_qouted <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}


#' Clean all lines out of the console.
#'
#' \code{env.clear_console}
#'
#'
#' Rstudio Server can get really slow if a lot of text, especially unicode
#'  or foreign data types are sitting in the GUI. This function is the equiv
#'  of \code{Edit > Ctrl + L}
#'
#' @family Environment utilities
#'
#'
#' @export
env.clear_console <- function(){
  cat("\014")
}


#' Restart the rsession from a function call
#'
#' \code{env.restart}
#'
env.restart <- function(){
  .rs.restartR()
}


#' Function to index every object in an environment or namespace
#'
#' \code{env.idx}
#'
#'
#' This function is helpful when space becomes cluttered, or even for finding
#' and calling alternate variables or functions from within a space
#'
#'
#' @param filter_class string or character vector, Pass the class of objects
#'  to be returned, if NULL, the default, return all.
#'
#' @param env string This variable interprets the input as a package name, or
#'  the default, global, the working invironment. This is the environment from
#'  which we want the objects from.
#'
#' @param pattern A regular expression or string to find in the environment.
#'
#'  @return
#' \format{
#'  A data frame with 77 observations on the following 2 variables.
#'  \describe{
#'    \item{\strong{obj_name}}{\emph{a character vector}
#'         This is the name of the object within the
#'         active, or called namespace.}
#'    \item{\strong{obj_type}}{\emph{a character vector}
#'         The class of the object. Such as character,
#'         data.frame, environment etc.}
#'   }
#' }
#'
#' @author Carl S.A. Boneri, \email{carl@@gmail.com}
#'
#'
#' @examples
#' ============  ========
#' obj_name      obj_type
#' ============  ========
#' .             function
#' aaply         function
#' adply         function
#' alply         function
#' amv_dimnames  function
#' a_ply         function
#' ============  ========
#'
#' @export
env.idx <- function(filter_class = NULL, env = 'global', pattern = NULL){

  if(env != 'global'){
    if(!grepl('^package', env)){
      get_from <- sprintf('package:%s', env)
    }else {
      get_from <- env
    }
  }else {
    get_from <- .GlobalEnv
  }

  var_jects <- ls(pos = get_from, all.names = TRUE)

  IDX <-
    ldply(var_jects, function(i){
      IST <-
        ifelse(env != 'global',
               class(get(i, pos = get_from)),
               class(get(i))
        )
      i_size <- object.size(deparse(substitute(i)))

      c(obj_name = i,
        obj_type = IST)
    })

  IDX$`obj_size(kb)` <- unlist(llply(1:nrow(IDX), function(i){
    object.size(get(IDX$obj_name[[i]])) * 0.001
  }))

  IDX$`obj_size(mb)` <- unlist(llply(1:nrow(IDX), function(i){
    object.size(get(IDX$obj_name[[i]])) * 1e-6
  }))


  if(!is.null(filter_class)){
    IDX <- IDX %>% filter(grepl(filter_class,obj_type)) %>%
      arrange(`obj_size(kb)`)
  }else {
    IDX <- IDX  %>%
      arrange(`obj_size(kb)`)
  }

  if(!is.null(pattern)){
    IDX <- IDX[grepl(pattern, IDX$obj_name),]
  }
  return(IDX)
}

#' Function for clearing all objects from cache in a session
#'
#'
#' \code{env.clear_objects}
#'
#'
#' Working in an environment or in an app can mean the workspace or
#' in-memory storage can get bogged down with unecessary data objects. This
#' can impact speed or simply clutter the space itself. This function will
#' remove all objects that are not functions in the current environment.
#'
#'
#' @family Environment utilities
#'
#' @examples
#'
#' # From a working environment
#' > sapply(ls(), function(i){
#'  is.function(get(i))
#' }) %>% {
#'   dput(names(.[!.]))
#' }
#'
#' [1] "nf" "nf.raw" "nf.team_df" "nf.team_df_master" "nf.teams"
#'
#' # Now clear anything beginning with nf followed by a dot and has a word after
#' # such as nf.team_df
#'
#' > env.clear_objects(drop_pat = "nf\\.(\\w+)+$")
#'
#' > sapply(ls(), function(i){
#'      is.function(get(i))
#'  }) %>% {
#'       (names(.[!.]))
#'  }
#'
#'  [1] "nf"
#'
#' @export
env.clear_objects <- function(keep_pat = NULL, drop_pat = NULL){

  all_in <- sapply(ls(envir = .GlobalEnv), function(i){
    is.function(get(i))
  })

  all_nonf <- as.character(names(all_in[!all_in]))

  if(!is.null(keep_pat)){
    all_nonf <- all_nonf[grepl(keep_pat, all_nonf, perl = TRUE, ignore.case = TRUE)]
  }
  if(!is.null(drop_pat)){
    all_nonf <- all_nonf[grepl(drop_pat, all_nonf, perl = TRUE, ignore.case = TRUE)]
  }

  rm(list = all_nonf, envir = .GlobalEnv)
}



#' Clear all created functions from an environment
#'
#' \code{env.clean_funs}
#'
env.clear_funs <- function(pat = NULL){
  all_f <- env.idx() %>% dplyr::filter(obj_type == 'function')
  if(!is.null(pat)){
    r_f <- grep(pat, all_f[[1]], perl = TRUE, ignore.case = TRUE, value = TRUE)
  }else {
    r_f <- all_d[[1]]
  }

  rm(list = r_f, envir = .GlobalEnv)
}
#' Create a new Global Environment
#'
#'
#' \code{env.new}
#'
#'
#' Works much like calling \code{\link{new.env()}} but allows passing a string
#' argument to assign to the environment namespace. Useful within function calls
#' where a new env can be used to mask operations and send outputs early to
#' outside functions while the intended function completes its steps.
#'
#'
#' @param env_name String, the name of the new environment.
#'
#'
#' @examples
#'
#' env.new('.hidden')
#'
env.new <- function(env_name = NULL){
  check_name <- tryCatch(is.character(env_name), error = function(e)FALSE)

  if(check_name){
    assign(env_name, new.env(), envir = .GlobalEnv)
  }else {
    set_name <- deparse(substitute(env_name))
    assign(set_name, new.env(), envir = .GlobalEnv)
  }
}



#' A wrapper to make everything silent
#'
#' \code{shh}
#'
#'
shh <- function(...){
  invisible(
    suppressWarnings(
      suppressPackageStartupMessages(
        suppressMessages(
          ...
        )
      )
    )
  )
}


files.get_ext <- function(path = NULL, ...){
  rgx <- '(\\.[A-z0-9]{1,})?\\.[^.]+$'
  part <- stringi::stri_extract_all_regex(path, rgx, simplify = TRUE)
  as.character(part)
}

files.has_ext <- function(path = NULL, ...){
  !is.na(files.get_ext(path))
}

#' Startup and global utilities
find.first <- function(edit = FALSE, show_lib = TRUE){

  candidates <- c(Sys.getenv("R_PROFILE"),
                  file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
                  Sys.getenv("R_PROFILE_USER"),
                  file.path(getwd(), ".Rprofile")
  )

  first_hit <- Filter(file.exists, candidates)

  if(show_lib & !edit){
    return(first_hit)
  }else {
    file.edit(first_hit)
  }
}

files.dl_log <- function(path_in = NULL){
  stri_replace_all_regex(path_in, '(\\.[A-z0-9]{1,})?\\.[^.]+$', '_log.txt')
}

files.read_log <- function(log_path = NULL){
  grep('[0-9]{1,}\\%',stri_read_lines(log_path), value = TRUE) %>%
    .[length(.)] %>% stri_extract_all_regex('[0-9]{1,}(?=\\%)') %>%
    as.numeric()
}

cmd.dl <- function(src_path = NULL, dest_path = NULL, .func = NULL){

  if(is.null(dest_path)){
    dest_path <- basename(src_path)
  }else if(!files.has_ext(dest_path)){
    dest_path <- sprintf('%s%s', dest_path, files.get_ext(src_path))
  }

  pat <- sprintf('-O %s %s', dest_path, src_path)

  # Check valid url and size
  h <- tryCatch(httr::HEAD(src_path), error = function(e)NA)

  if(inherits(h, 'response') && identical(status_code(h), 200L)){

    dl_size <- 0.00000095367432 * as.numeric(h[['headers']][['content-length']])

    call_pat <- sprintf('wget -bc %s -o %s', pat, files.dl_log(dest_path))
    cl <- system(call_pat, ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
}
#' Download a file from the internet and copy to a local directory
#'
#' \code{files.download}
#'
#'
files.download <- function(src_path = NULL, dest_name = NULL, dest_path = NULL,
                           multi = FALSE, in_background = TRUE){
  if(!is.null(dest_name)){
    dest_name <- basename(src_path)
  }
  if(!is.null(dest_path)){
    dest_path <- sprintf('%s/%s', dest_path, dest_name)
  }else {
    dest_path <- sprintf('~/%s', dest_name)
  }

  # If downloading from a remote server, ensure the file returns a 200 response.
  if(identical(attr(curlGetHeaders(get_path),"status"),200L)){
    curl::curl_download(src_path, destfile = dest_path)
  }

}


#' Download and unzip a file from the internet or other external resource
#'
#' \code{files.unzip}
#'
#'
#'
#' @param zip_location The url, or directory path pointing to the
#'  location of the source zip file.
#'
#' @param exdir The directory to extract the the contents of the zip folder to.
#'
#' @param pattern If set, this is a regular expression pattern to be used
#'  for telling the function which files to keep. The side-effect is that all
#'  files not matching will be discarded.
#'
#'
files.unzip <- function(zip_path = NULL, dest_path = NULL, keep_src = FALSE, ...){

  if(grepl('^http', zip_path)){

    td <- tempdir()
    tf <- tempfile(fileext = ".zip", tmpdir = td)
    curl::curl_download(zip_path, destfile = tf)
    if(file.exists(tf)){
      zip_path <- tf
    }
  }

  if(is.null(dest_path)){
    dest_path <- getwd()
  }

  unzip(zipfile = zip_path, exdir = dest_path)

}





