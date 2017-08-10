docr.check_and_load <- function(){
  if(dir.exists("~/roxydoc2")){
    all_rdocr <- list.files('~/roxydoc2/R',
                            full.names = TRUE,
                            recursive = TRUE,
                            pattern = "[^.r]$",
                            all.files = TRUE)

    shh(lapply(all_rdocr, function(i){
      source(i, echo = FALSE, verbose = FALSE)
    })) %>% {
      if(exists("docr.defaults")){
        if(!docr.has_defaults()){
          docr.defaults_prompt()
        }
      }
    }
  }else {
    if(!exists('docr.remote_source')){
      src_doc <- devtools::source_url(
        "https://raw.githubusercontent.com/CarlBoneri/roxydoc2/master/R/docr_remote_source.R"
      )
      docr.remote_source() %>% {
        if(!docr.has_defaults()){
          docr.defaults_prompt()
        }
      }
    }
  }
}