#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

#install.packages("spocc")
library(spocc)

parasite <- read.xlsx("data/01_Zoonotic_Microparasite_List.xlsx")

parasite<-parasite%>%select(ParasiteCurrentName)%>%filter(ParasiteCurrentName!="not_found")

round1 <- parasite %>% select(ParasiteCurrentName) %>% slice(2:5)
parasite=as.data.frame(round1[,"ParasiteCurrentName"])
names(parasite)="parasite"
my_spp <- as.vector(paste(parasite$parasite))
my_list<-split(my_spp, my_spp)

for (i in my_list) {
  occur <- occ(query = my_list[[i]], from = 'gbif', gbifopts = list(country = 'BR'))
  occur.gbif <- occur$gbif #output da busca
  gbif_data <- occur.gbif[["data"]] 
  gbif_data <- occ2df(occur.gbif) #convertendo a lista em data.frame
  write.csv(gbif_data, file=paste("data",i, "GBIF.csv", sep="_"))
}
