% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docr_wrap.R
\name{docr.wrap}
\alias{docr.wrap}
\title{Properly wrap free-hand RD text}
\usage{
docr.wrap(x = NULL, gap_btw_lines = TRUE, pre_fix = "#'", file = NULL,
  assign_as = NULL)
}
\description{
\code{docr.wrap}
}
\details{
This is text
 t
}
\examples{
# Created a line of body text with formatting
my_text <- paste0(collapse=" ",
             c('A function which allows passing',
               docr.txt('italics','external'),
               'arguments in the form of',
               docr.txt('code','alist')))

> docr.wrap(my_text)
# #'  A function which allows passing \\\\emph{external} arguments
# #'   in the form of \\\\code{alist}

}
