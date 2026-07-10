# utils_iucn.R
library(iucnredlist)
library(dplyr)
library(stringr)

clean_name <- function(x){
  x |>
    str_replace_all("_"," ") |>
    str_replace_all("\ufb01","fi") |>
    str_replace_all("\ufb02","fl") |>
    str_squish() |>
    word(1,2)
}

retry_api <- function(expr, max_retry=8){
  i <- 1
  repeat{
    out <- tryCatch(force(expr), error=function(e)e)
    if(!inherits(out,"error")) return(out)
    msg <- conditionMessage(out)
    if(grepl("429", msg) && i < max_retry){
      Sys.sleep(min(60,5*i))
      i <- i + 1
      next
    }
    stop(out)
  }
}

choose_assessment <- function(x){
  if(nrow(x)==0) return(x)
  x |>
    filter(scopes_code=="1") |>
    arrange(desc(latest), desc(as.integer(year_published)))
}
