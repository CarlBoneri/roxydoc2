#' This is a boilerplate meta template for working with private or patent projects.
#'
#' \code{docr.meta_private}
#'
#' Any meta information, or function that by default returns a
#'  pre-formatted meta block of text to be included in the head
#'  or footer of a script file.
#'
#'
#' @family Documentation functions
#' @export
docr.meta_private <- function(user_name = NULL,
                              user_email = NULL,
                              org_name = NULL,
                              org_website = NULL){

  pound <- function(n = NULL, ...){
    stri_join(rep("#", n), collapse = "")
  }

  the_year <- strftime(Sys.Date(),"%Y")

  if(!is.null(getOption('docr'))){
    all_docr <- docr.get_defaults()
    user_name <- all_docr$author
    user_email <- all_docr$email
    org_name <- all_docr$company
    org_website <- all_docr$website
  }
  # The Boiler -----
  the_meta <-
    c(sprintf(
      " Copyright (C) %s %s; %s - All Rights Reserved",
      the_year, user_name, org_name
    ),
    "",
    " You may use, distribute and modify this code under the",
    " terms found in the LICENSE file which you should have",
    " received with copy of this document. If not, please write to:",
    sprintf(" %s, or visit %s.",user_name, org_website),
    "",
    "",
    " NOTICE: All information contained herein is, and remains",
    sprintf(" the property of %s, %s and its subsidiaries.", user_name, org_name),
    " The intellectual and technical concepts contained",
    sprintf(" herein are proprietary to %s, %s", user_name, org_name),
    " and its subsidiaries; and may be or are currently covered by U.S.",
    " and Foreign Patents, patents in process, and are therefore are",
    " protected by trade secret, business method patents or copyright law.",
    "",
    " Dissemination of this information or reproduction of this material",
    " is strictly forbidden unless prior written permission is obtained",
    sprintf(" from %s<%s> on behalf of %s ",
            user_name, user_email, org_name),
    " and its subsidiaries; or any legal divestiture thereof."
    )
  # The wrap -----
  meta_body <- paste0(pound(2), the_meta)
  all_meta <- structure(meta_body, class = "doc_meta")


  # End docr.meta ----
  return(all_meta)
}
