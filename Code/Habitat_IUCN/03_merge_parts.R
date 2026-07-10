# 03_merge_parts.R
library(dplyr)
files <- list.files(pattern="^IUCN_results_part[1-5]\\.csv$", full.names=TRUE)
res <- bind_rows(lapply(files, read.csv, stringsAsFactors=FALSE)) |>
  distinct(species_original, .keep_all=TRUE)
write.csv(res,"IUCN_results_all.csv",row.names=FALSE)


library(tidyr)
library(dplyr)

habitats_long <-
  results |>
  separate_rows(habitats, sep=";;")