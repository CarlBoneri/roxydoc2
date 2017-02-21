#' Move all docr files into the working projects revdep folder
#'
#'
docr.freetype <- function(file_name = NULL, assign_as = NULL){

  raw <- readline(prompt = ":")

  if(!is.null(file_name)){
    if(!file.exists(file_name)){
      file.create(file_name)
    }
  }
}