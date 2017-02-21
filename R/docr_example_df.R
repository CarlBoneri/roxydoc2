#' Print a data frame for RD documentation in examples
#'
#'
#' \code{docr.example_df}
#'
#'
#'
#'
#' @author Carl S.A. Boneri, \email{carl.boneri@@whyles.com}
#'
#' @family Documentation functions
#'
#'
#'
#' @examples
#'
#'  > docr.example_df(mtcars)
#'
#'  #'=================  ====  ===  ====  ===  ====
#'  #'row_idx             mpg  cyl  disp   hp  drat
#'  #'=================  ====  ===  ====  ===  ====
#'  #'Mazda RX4          21.0    6   160  110  3.90
#'  #'Mazda RX4 Wag      21.0    6   160  110  3.90
#'  #'Datsun 710         22.8    4   108   93  3.85
#'  #'Hornet 4 Drive     21.4    6   258  110  3.08
#'  #'Hornet Sportabout  18.7    8   360  175  3.15
#'  #'Valiant            18.1    6   225  105  2.76
#'  #'=================  ====  ===  ====  ===  ====
#'
#'  # NOW SET COMMENTS ON THE DATA FRAME ITSELF
#'
#' > docr.example_df(mtcars, comments  = "these are cars")
#'	# these are cars
#'
#'	=================  ====  ===  ====  ===  ====
#'	row_idx             mpg  cyl  disp   hp  drat
#'	=================  ====  ===  ====  ===  ====
#'	Mazda RX4          21.0    6   160  110  3.90
#'	Mazda RX4 Wag      21.0    6   160  110  3.90
#'	Datsun 710         22.8    4   108   93  3.85
#'	Hornet 4 Drive     21.4    6   258  110  3.08
#'	Hornet Sportabout  18.7    8   360  175  3.15
#'	Valiant            18.1    6   225  105  2.76
#'	=================  ====  ===  ====  ===  ====
#' @export
docr.example_df <- function(df, just_head = T, comments = NULL, write_fun = NULL){

  if(length(df) > 7){
    DF <- df[,1:5]
  }else {
    DF <- df
  }

  if(just_head){
    DF <- head(DF)
  }else{
    DF <- DF
  }


  # In the event a matrix structure contains row named variables
  #

  if(any(grepl('[A-z]', row.names(DF)))){
    DF <- data.frame(row_idx = row.names(DF), DF, stringsAsFactors = FALSE)
    row.names(DF) <- NULL
  }else {
    DF <- DF
  }

  kabled <- llply(apply(DF,2,function(i){
    a <- stri_sub(i, 1, 7)

    ifelse(nchar(i) > 7, sprintf('%s ...', a), a)

  }) %>% as.data.frame() %>% kable('rst'), function(ii){
    sprintf("#' %s", ii)
  }) %>% paste.html

  if(!is.null(write_fun)){
    fun_line <- sprintf("#' > %s\n#'", write_fun)
    kabled <- c(fun_line, kabled)
  }

  if(!is.null(comments)){
    set_df <- c(
      sprintf("#' # %s", comments),
      kabled
    )
  }else {
    set_df <- kabled
  }

  kabled <- c("#'",set_df,"#'\n")



  HTML(paste0(kabled,collapse= "\n"))

}
