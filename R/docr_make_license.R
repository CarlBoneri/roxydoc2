#' Generate a LICENSE formatted to Rd specs
#'
#' \code{docr.make_lic}
#'
#'@param owner_name string Default to NULL Owner of the License.
#'@param owner_org string Default to NULL If the owner attributes
#'  work to an Organization.
#'@param year character Default to current year.
#'@param output_to call Default to file and console Where should the output
#' print?
#'
#'
#' @return Output is either a LICENSE file object in the local or working
#'  directory or printed to the console
#'
#' @examples
#' doc.make_lic(owner_name = 'Carl Boneri',owner_org = "FAS Advisory",
#' output_to = 'file')
#' #> #'@param owner_name string Default to NULL
#' #> #'@param owner_org string Default to NULL
#' #> #'@param year character Default to current
#' #> #'@param output_to call Default to file and console
#'
#'
#'
#' @family Documentation functions
#' @export
docr.make_license <- function(owner_name = NULL,
                              owner_org = NULL,
                              year = 'current',
                              output_to = c('file','console')){

  if(year != 'current'){
    yr <- as.numeric(year)
  }else{
    yr <- as.numeric(strftime(Sys.Date(), "%Y"))
  }
  yr_out <- paste0('YEAR:',yr)


  all_docrs <- docr.get_defaults()

  if(is.null(owner_name)){
    doc_authr <- all_docrs$author
    if(!is.null(doc_authr)){
      owner_name <- doc_authr
    }else{
      owner_name <- stringi::stri_trans_totitle(
        readline('Owner name for License?')
      )
    }
  }

  owner_parse <- paste0('COPYRIGHT HOLDER:',owner_name)

  if(is.null(owner_org)){
    docr_org <- all_docrs$company
    if(!is.null(docr_org)){
      owner_org <- docr_org
    }else{
      owner_org <- stringi::stri_trans_totitle(
        readline('Organization/Company name for License?')
      )
    }
  }


  copy_out <- paste(owner_parse,owner_org,sep="; " )

  cat_form <- paste(yr_out,copy_out,sep = "\n")

  if(output_to == 'file'){
    tmp_name <- "LICENSE"
    writeLines(cat_form, con = tmp_name)
    #   if(output_to == 'file'){
    #     zz <- file("LICENSE", "w")  # open an output file connection
    #     cat(yr_out,copy_out, file = zz, sep = "\n")
    #     close(zz)
  }else{
    cat(cat_form)
  }
}
