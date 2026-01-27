#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

# Busca na base PubMed

# A busca deve ser feita com subconjuntos da lista de especies 

# load library
library(rentrez)
library(tidyverse)
library(openxlsx)

species <- read.xlsx("data/03_Brazilian_Mammal_List.xlsx")
round1 <- species %>% select(HostSpecies_SBMz) %>% slice(3:5)
species=as.data.frame(round1[,"HostSpecies_SBMz"])
names(species)="species"
my_spp <- as.vector(paste(species$species, "[TITL] AND Brazil[TITL]"))
my_list<-split(my_spp, my_spp)
my_list

for (i in my_list) {
  
  require(rentrez)
  set_entrez_key("c262c38ab7cd4b40fd84389b1e38dc244308") # insert your entrez key
  
  
  search <- lapply(my_list,entrez_search,db ="pubmed") 
  
  if(length(search[[i]]$ids != 0)) {
    
    multi_summs <- lapply(search[[i]]$ids, entrez_summary,db="pubmed")
    
    date_and_cite <- as.matrix(lapply(multi_summs, extract_from_esummary, c("uid", "sortfirstauthor", 
                                                                            "pubdate", "title")))
    
    
    extract_list <- function(x){
      date_and_cite[[x]]
    }
    
    date_and_cite <-  map_df(1:length(date_and_cite),extract_list)
    
    
    
    write.csv(date_and_cite, file=paste("citations",i, "pubmed.csv", sep="_"))
    
    
    print("YEY! looking good ;)")
    
  }
  
  else { 
    
    print("Sorry girl, your search returned no hits")
    
  }
}

getwd()
path.pubmed<-"parasite-host-Brazil"
files.pubmed<-list.files(path=path.pubmed, pattern="*.csv", full.names = T, recursive=T) 
f.cite.pubmed <- lapply(files.pubmed, read.csv)
cite.pubmed = do.call('rbind', f.cite.pubmed) 

cite.pubmed = cite.pubmed %>% select(sortfirstauthor, pubdate, title) %>% distinct()

write.csv(cite.pubmed, file = "pubmed.citations.csv")

