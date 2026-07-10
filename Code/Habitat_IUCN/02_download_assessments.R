# 02_download_assessments.R
library(dplyr)
library(purrr)
source("C:/Users/gabri/Downloads/utils_iucn.R")

api <- init_api("EBmhupfazayW9DwB5efJkJkGYsBBk5nAMVQH")
parte <- 1

ids <- read.csv(paste0("assessment_ids_part",parte,".csv"))

out <- vector("list", nrow(ids))

for(i in seq_len(nrow(ids))){
 raw <- retry_api(assessment_data(api, ids$assessment_id[i]))
 
 safe_value <- function(tbl, column){
   
   if(is.null(tbl))
     return(NA_character_)
   
   if(!inherits(tbl, "data.frame"))
     return(NA_character_)
   
   if(nrow(tbl)==0)
     return(NA_character_)
   
   if(!(column %in% names(tbl)))
     return(NA_character_)
   
   value <- tbl[[column]][1]
   
   if(length(value)==0)
     return(NA_character_)
   
   value
   
 }
 
 safe_concat <- function(tbl, column){
   
   if(is.null(tbl))
     return(NA_character_)
   
   if(!inherits(tbl, "data.frame"))
     return(NA_character_)
   
   if(nrow(tbl)==0)
     return(NA_character_)
   
   if(!(column %in% names(tbl)))
     return(NA_character_)
   
   paste(
     unique(na.omit(tbl[[column]])),
     collapse=";;"
   )
   
 }
 
 
 p <- parse_assessment_data(raw)

 out[[i]] <- data.frame(
   species_original = ids$species_original[i],
   species_used = ids$species_used[i],
   assessment_id = ids$assessment_id[i],
   category = safe_value(
     p$red_list_category,
     "code"
   ),
   
   criteria = safe_value(
     p$criteria,
     "value"
   ),
   
   habitats = safe_concat(
     p$habitats,
     "description"
   ),
   
   threats = safe_concat(
     p$threats,
     "description"
   )
 )
 write.csv(bind_rows(out),
   paste0("IUCN_results_part",parte,".csv"),
   row.names=FALSE)
}
