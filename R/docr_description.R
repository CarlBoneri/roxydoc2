################################################################################
## This file is part of the "ytweetr" package for R.
## Copyright (C) 2018, Carl Boneri
## All rights reserved.
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## 1. Redistributions of source code must retain the above copyright notice,
## this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## 3. Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
## BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
## FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
## EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
################################################################################
#' Create a new DESCRIPTION.dcf file
#'
#' \code{docr.package_description}
#'
#' Function to assist and save time with the population of the DESCRIPTION file
#'
#'
#' @param package Name of the package the DESCRIPTION file belongs to
#'
#' @param add_license Should a license file be created
#'
#' @param ... Fields to add to the description list. Important: names must be
#'   \strong{camelCase}.
#'
#'
#'
#' @author Carl Boneri, \email{carl.boneri@@whyles.com}
#'
#'
#' @examples
#'
#'
#' @export

docr.package_description <- function(package = NULL, add_license = TRUE, ...){
  if(is.null(package)){
    package <- basename(getwd())
  }

  # New Items
  desc_list <- devtools:::build_description(name = package, extra = list(...))
  # Old Items
  if(devtools:::has_description(file.path(path.expand("~"), package))){
    desc_list <- merge(devtools:::read_dcf(path = "DESCRIPTION"), desc_list)
  }

  # if(add_license){
  #   docr.make_license()
  # }
  desc_list
}



#' Startup functions for a new package
#'
#' \code{docr.new_package}
#'
#' This will setup all the standard needs for a new package, such as the populating
#'   the \code{DESCRIPTION} file, adding \code{LICENSE}, package directory structure,
#'   \code{Vignettes} etc.
#'
#'
#' @param ...
#'
#'
#'
#' @author Carl Boneri, \email{carl.boneri@@whyles.com}
#'
#'
#' @examples
#'
#'
#' @export

docr.new_package <- function(package = NULL, add_vignettes = TRUE, ){
  devtools:::write_dcf()
}
