roxydoc
================

roxydoc2 is intended as a set of useful global tools for R package authors to use when developing packages. It provides a series of wrapper and utility functions aimed at supplementing the devtools and roxygen2 packages.

Implementations
---------------

1.  Set global author variables for package docs
2.  Allow for flexible code creation, at the console or in a script with the focus of documentation being the last thought
3.  Properly format all latext and markdown output strings
4.  Detailed data and output descriptions from actual code outputs
5.  Addition of any copyright notices, or boilerplate to each script
6.  Organization of code even when the author fights it...
7.  Work at the console as much as you'd like, with script compiling running in the background

### Setting the global variables for the author

```{r}
o <- lapply(list.files("R", pattern = "docr_", full.names=TRUE), source)
docr.defaults(author = "Joe Shmow", 
              email = "joe.s@emails.com", 
              company = "Joe's cRab shack", 
              website = "https://joeknowsr.com", 
              github = "JoeShmow")
```

This will then store the data to the .GlobalEnv as

```{r}
knitr::kable(docr.get_defaults())
```

| author    | email              | company          | website                 | github   |
|:----------|:-------------------|:-----------------|:------------------------|:---------|
| Joe Shmow | <joe.s@emails.com> | Joe's cRab shack | <https://joeknowsr.com> | JoeShmow |

So now when `docr.author` is called:

```{r}
docr.author()
```

    #' @author Joe Shmow, \email{joe.s@@emails.com}

### Documenting datasets

The primary purpose of R is generally to create, or manipulate raw data. Often it is time consuming and tedious to properly format what the dataset looks like, or contains. These functions aim to ease the burdon.

#### dataframes

```{r}
raw <- capture.output(docr.describe_df(df_in = mtcars, include_slice = TRUE))
cat(raw, sep = "\n")
```


    #'
    #' \format{
    #'  A data frame with 32 observations on the following 11 variables.
    #'  \describe{
    #'    \item{\strong{mpg}}{\emph{numeric} formatted vector with \emph{25} unique values.}
    #'    \item{\strong{cyl}}{\emph{numeric} formatted vector with \emph{3} unique values.}
    #'    \item{\strong{disp}}{\emph{numeric} formatted vector with \emph{27} unique values.}
    #'    \item{\strong{hp}}{\emph{numeric} formatted vector with \emph{22} unique values.}
    #'    \item{\strong{drat}}{\emph{numeric} formatted vector with \emph{22} unique values.}
    #'    \item{\strong{wt}}{\emph{numeric} formatted vector with \emph{29} unique values.}
    #'    \item{\strong{qsec}}{\emph{numeric} formatted vector with \emph{30} unique values.}
    #'    \item{\strong{vs}}{\emph{numeric} formatted vector with \emph{2} unique values.}
    #'    \item{\strong{am}}{\emph{numeric} formatted vector with \emph{2} unique values.}
    #'    \item{\strong{gear}}{\emph{numeric} formatted vector with \emph{3} unique values.}
    #'    \item{\strong{carb}}{\emph{numeric} formatted vector with \emph{6} unique values.}
    #'   }
    #' }

#### lists

```{r}
raw2 <- capture.output(docr.list_named(as.list(sapply(as.list(mtcars),`[[`,1))))
cat(raw2, sep = "\n")
```

    #' \describe{
    #'   \item{mpg}{21}
    #'   \item{cyl}{6}
    #'   \item{disp}{160}
    #'   \item{hp}{110}
    #'   \item{drat}{3.9}
    #'   \item{wt}{2.62}
    #'   \item{qsec}{16.46}
    #'   \item{vs}{0}
    #'   \item{am}{1}
    #'   \item{gear}{4}
    #'   \item{carb}{4}
    #' }

### And to add boilerplate to all files:

```{r}
cat(docr.meta_private(),sep = "\n")
```

    ################################################################################
    ## Copyright (C) 2017 Joe Shmow; Joe's cRab shack - All Rights Reserved
    ##
    ## You may use, distribute and modify this code under the
    ## terms found in the LICENSE file which you should have
    ## received with copy of this document. If not, please write to:
    ## Joe Shmow, or visit https://joeknowsr.com.
    ##
    ##
    ## NOTICE: All information contained herein is, and remains
    ## the property of Joe Shmow, Joe's cRab shack and its subsidiaries.
    ## The intellectual and technical concepts contained
    ## herein are proprietary to Joe Shmow, Joe's cRab shack
    ## and its subsidiaries; and may be or are currently covered by U.S.
    ## and Foreign Patents, patents in process, and are therefore are
    ## protected by trade secret, business method patents or copyright law.
    ##
    ## Dissemination of this information or reproduction of this material
    ## is strictly forbidden unless prior written permission is obtained
    ## from Joe Shmow<joe.s@emails.com> on behalf of Joe's cRab shack 
    ## and its subsidiaries; or any legal divestiture thereof.
    ################################################################################

### And writing code at the console and prepping for doc/scripts:

```{r}
docr.clean_fun <- function(fun_name = NULL, trim_blanks = FALSE,
                           rm_comments = FALSE, do_cat = TRUE){
  deparse(get(fun_name), control = "all") %>% (function(x){
    x[[1]] <- sprintf("%s <- %s",fun_name, x[[1]])

    if(trim_blanks){
      x <- x[nchar(x) > 0]
    }

    if(rm_comments){
      x <- grep("^[[:space:]]{1,}\\#", x, invert = TRUE, value = TRUE)
    }
    if(do_cat){
      cat(x, sep = "\n")
    }else {
      x
    }
  })
}
this.fun <- function(x = 1, y = 2,...){
    x * y
}
Fun <- docr.clean_fun('this.fun',do_cat = FALSE)
Args <- docr.param('this.fun')
Auth <- capture.output(docr.author())
Meta <- docr.meta_private()
cat(c(Meta,sprintf("#' %s",docr.fun('this.fun')),
      Args[[1]], sprintf("#' %s\n", Args[2:length(Args)]), Auth, Fun),sep = "\n")
```

    ################################################################################
    ## Copyright (C) 2017 Joe Shmow; Joe's cRab shack - All Rights Reserved
    ##
    ## You may use, distribute and modify this code under the
    ## terms found in the LICENSE file which you should have
    ## received with copy of this document. If not, please write to:
    ## Joe Shmow, or visit https://joeknowsr.com.
    ##
    ##
    ## NOTICE: All information contained herein is, and remains
    ## the property of Joe Shmow, Joe's cRab shack and its subsidiaries.
    ## The intellectual and technical concepts contained
    ## herein are proprietary to Joe Shmow, Joe's cRab shack
    ## and its subsidiaries; and may be or are currently covered by U.S.
    ## and Foreign Patents, patents in process, and are therefore are
    ## protected by trade secret, business method patents or copyright law.
    ##
    ## Dissemination of this information or reproduction of this material
    ## is strictly forbidden unless prior written permission is obtained
    ## from Joe Shmow<joe.s@emails.com> on behalf of Joe's cRab shack 
    ## and its subsidiaries; or any legal divestiture thereof.
    ################################################################################
    #' \code{\link[roxydoc2]{this.fun}}
    @param x
    #' @param y

    #' @param ...

    #' @author Joe Shmow, \email{joe.s@@emails.com}
    this.fun <- function(x = 1, y = 2,...){
        x * y
    }
