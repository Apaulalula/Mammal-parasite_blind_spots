# 01_find_assessment_ids.R
library(readxl)
library(dplyr)
library(purrr)
source("C:/Users/gabri/Downloads/utils_iucn.R")

api <- init_api("EBmhupfazayW9DwB5efJkJkGYsBBk5nAMVQH")
parte <- 1
n_partes <- 5

dat <- read_xlsx("C:/Users/gabri/Downloads/data06-BMPO-Data.xlsx") |>
 transmute(
   species_original=clean_name(HostSpecies_SBMz),
   synonym=clean_name(HostMach_COL_SiBBr)
 ) |> distinct()

idx <- split(seq_len(nrow(dat)),
             cut(seq_len(nrow(dat)), breaks=n_partes, labels=FALSE))
dat <- dat[idx[[parte]],]

ids <- list()
not_found <- list()
failed <- list()

for(i in seq_len(nrow(dat))){
  ok <- FALSE
  for(sp in unique(na.omit(c(dat$species_original[i], dat$synonym[i])))){
    g <- word(sp,1); s <- word(sp,2)
    res <- tryCatch(retry_api(assessments_by_name(api,g,s)), error=function(e)e)
    if(inherits(res,"error")) next
    res <- choose_assessment(res)
    if(nrow(res)==0) next
    ids[[length(ids)+1]] <- data.frame(
      species_original=dat$species_original[i],
      species_used=sp,
      assessment_id=res$assessment_id[1]
    )
    write.csv(bind_rows(ids),
      paste0("assessment_ids_part",parte,".csv"), row.names=FALSE)
    ok <- TRUE
    break
  }
  if(!ok){
    not_found[[length(not_found)+1]] <- dat[i,]
    write.csv(bind_rows(not_found),
      paste0("Species_not_found_part",parte,".csv"), row.names=FALSE)
  }
  Sys.sleep(3)
}
