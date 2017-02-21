#' Persistent global options for the docr family
#'
#' \code{docr.defaults}
#'
#' In short form this is a helper to always include author, or additional
#' params on each script skeleton. The idea is to create a global and default
#' value emitting function stored on an \code{\link{options}} variable
#' so that each script can include boilerplate...if desired. The settings for
#' these options are:
#' \describe{
#'   \item{author}{The name of the document, package, or script author
#'    }
#'   \item{email}{The email address of the author to be included in the author
#'      title line.
#'    }
#'    \item{company}{Any company or corporate name associated with the package.
#'      This is moreso for development of proprietary packages that may be
#'      hosted or viewed by a general audience but whose contents, scripts,
#'      or algorithms may be patent-protected.
#'    }
#'    \item{website}{Any url to be included for author reference or package
#'      homepage.
#'    }
#'    \item{github}{If a github account is persistent for the package or user,
#'      This would be the username or organizational name for the package, user,
#'      author or entity that is wished to be associated with the package.
#'    }
#' }
#'
#' @family Documentation functions
#'
#' @export
docr.defaults <- function(author = NULL, email = NULL, company = NULL,
                          meta = NULL, website = NULL, github = NULL){

  docr_opts <- list(
    author = author,
    email = email,
    meta = meta,
    company = company,
    website = website,
    github = github
  )

  docr_set <- docr_opts[!mapply(is.null,docr_opts)]

  options(docr = docr_set)
}
