#' Boilerplate header meta for scripts and source code for open-source packages
#'
#'
#' \code{docr.meta_public}
#'
#'
#'
#' @family Documentation functions
#'
#' @export
docr.meta_public <- function(package_name = NULL, authors = NULL, years = NULL,
                             to_file = NULL){
  if(is.null(package_name)){
    package_name <- basename(getwd())
  }
  if(is.null(authors)){
    if(!is.null(getOption('docr'))){
      authors <- docr.get_defaults()[['author']]
    }else{
      authors <- stri_trans_totitle(Sys.info()[['user']])
    }
  }
  if(is.null(years)){
    years <- strftime(Sys.Date(),"%Y")
  }

  the_meta <- c(sprintf('This file is part of the "%s" package for R.',package_name),
                sprintf('Copyright (C) %s, %s',years, authors),
                'All rights reserved.',
                'Redistribution and use in source and binary forms, with or without',
                'modification, are permitted provided that the following conditions are met:',
                '',
                '1. Redistributions of source code must retain the above copyright notice,',
                'this list of conditions and the following disclaimer.',
                '',
                '2. Redistributions in binary form must reproduce the above copyright notice,',
                'this list of conditions and the following disclaimer in the documentation',
                'and/or other materials provided with the distribution.',
                '',
                '3. Neither the name of the copyright holder nor the names of its',
                'contributors may be used to endorse or promote products derived from',
                'this software without specific prior written permission.',
                '',
                'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS',
                '"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,',
                'BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS',
                'FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT',
                'HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,',
                'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,',
                'PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;',
                'OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,',
                'WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE',
                'OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,',
                'EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.')

  meta_body <- paste0(pound(2)," ",the_meta)

  all_meta <- paste0(c(pound(80), meta_body, pound(80)), collapse = "\n")
  if(!is.null(to_file)){
    cat(all_meta, file = to_file)
  }else {
    return(all_meta)
  }
}
