#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

# Busca na base nucleotide do NCBI e referencia cruzada com pubmed

# load library
#install.packages("rentrez")
library(rentrez)
library(tidyverse)
library(openxlsx)

parasite <- read.xlsx("data/01_Zoonotic_Microparasite_List.xlsx")

parasite<-parasite%>%select(ParasiteCurrentName)%>%filter(ParasiteCurrentName!="not_found")

round1 <- parasite %>% select(ParasiteCurrentName) %>% slice(1:3)
parasite=as.data.frame(round1[,"ParasiteCurrentName"])
names(parasite)="parasite"
my_spp <- as.vector(paste(parasite$parasite, "[WORD] AND Brazil[WORD]"))
my_list<-split(my_spp, my_spp)
my_list
set_entrez_key("xxxx") # insert your entrez key

for (i in my_list) {
  
  require(rentrez)
  
  search <- lapply(my_list,entrez_search,db ="nucleotide", retmax=300) 
  
  if(length(search[[i]]$ids != 0)) {
    
    
    multi_summs <- lapply(search[[i]]$ids, entrez_summary,db="nucleotide")
    
    organism <- lapply(multi_summs, extract_from_esummary, c("uid", "caption", "title", "createdate",
                                                             "updatedate", "taxid", "subtype", "subname", 
                                                             "organism"))
    
    organism <- do.call("rbind", lapply(organism, as.data.frame)) 
    
    
    write.csv(organism, file=paste("data",i, "nucleotide.csv", sep="_"))
    
    
    
    # PESQUISA CRUZADA COM PUBMED #
    
    
    
    pubmed_links <- entrez_link(dbfrom="nucleotide", id=search[[i]]$ids, db="pubmed")
    
    pubmed_ids <- as.vector(c(pubmed_links$links$nuccore_pubmed))
    
    multi_summs <- lapply(pubmed_ids, entrez_summary,db="pubmed")
    
    
    date_and_cite <- as.matrix(lapply(multi_summs, extract_from_esummary, c("uid", "sortfirstauthor", 
                                                                            "pubdate", "title")))
    
    
    extract_list <- function(x){
      date_and_cite[[x]]
    }
    
    date_and_cite <-  map_df(1:length(date_and_cite),extract_list)
    
    
    
    write.csv(date_and_cite, file=paste("crossref",i, "pubmed.csv", sep="_"))
    
    
    
    print("YEY! looking good ;)")
    
  }
  
  else { 
    
    print("Sorry girl, your search returned no hits")
    
  }
}


