# Create a holding environment
# edoc <- new.env()

vargs.ensure_quote <- function(x){
  tryCatch(is.character(x), error = function(e){
    as.character(deparse(substitute(x)))
  })
}
#' Function to create new environments
env.new <- function(env_name = NULL){
  check_name <- tryCatch(is.character(env_name), error = function(e)FALSE)

  if(check_name){
    assign(env_name, new.env(), envir = .GlobalEnv)
  }else {
    set_name <- deparse(substitute(env_name))
    assign(set_name, new.env(), envir = .GlobalEnv)
  }
}

#' Check for the environment
#'
env.true <- function(env_name = NULL, where = .GlobalEnv){
  chk <- ls(envir = where, pattern = env_name, all.names = TRUE)
  if(!length(chk)){
    FALSE
  }else {
    TRUE
  }
}