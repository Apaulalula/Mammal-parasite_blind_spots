#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

# Busca na base nucleotide do NCBI #

# A busca foi pensada em casos com registros muito numerosos
# A busca deve ser feita especie por especie
# O numero da especie para a qual a busca deve ser feita esta definido na linha que cria 
# o objeto round 1, dentro da funcao slice(x)

# load library
#install.packages("rentrez")
library(rentrez)
library(tidyverse)
library(openxlsx)

parasite <- read.xlsx("data/01_Zoonotic_Microparasite_List.xlsx")

parasite<-parasite%>%select(ParasiteCurrentName)%>%filter(ParasiteCurrentName!="not_found")

round1 <- parasite %>% select(ParasiteCurrentName) %>% slice(2)
parasite=as.data.frame(round1[,"ParasiteCurrentName"])
names(parasite)="parasite"
my_spp <- as.vector(paste(parasite$parasite, "[WORD] AND Brazil[WORD]"))
my_list<-split(my_spp, my_spp)
my_list


search <- lapply(my_list,entrez_search,db ="nucleotide", retmax=64000, use_history=TRUE) 
length(search[[1]]$ids != 0)
str(search)

recs<- vector(mode="list")

for(seq_start in seq(1,length(search[[1]]$ids != 0),100))
{
  recs2 <- entrez_summary(db="nucleotide", web_history=search[[1]]$web_history, retmax=100, retstart=seq_start-1)
  recs<- append(x=recs,values=recs2,after=seq_start)
}

str(lengths(recs))

organism <- lapply(recs, extract_from_esummary, c("uid", "caption", "title", "createdate",
                                                  "updatedate", "taxid", "subtype", "subname", 
                                                  "organism"))

organism <- do.call("rbind", lapply(organism, as.data.frame)) 

write.csv(organism, file=paste("data",my_spp, "nucleotide.csv", sep="_"),row.names=seq(1:nrow(organism)))


