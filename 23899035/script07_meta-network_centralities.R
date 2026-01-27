#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

# load library
library(tidyverse)
library(openxlsx)
library(igraph)

interacoes <- read.xlsx("data/06_BMPO_Data.xlsx") 

interacoes <- interacoes %>% filter(ZoonoticStatus == "1")
interacoes <- interacoes %>% filter(!ParGroup %in% c("Helminth", "Arthropod"))

interacoes <- interacoes %>% filter(ParSpecies != "not_reported")
interacoes <- interacoes %>% filter(ParSpecies != "not_found")

interacoes <- interacoes %>% filter(HostSpecies_SBMz != "not_found")

int.biome <- interacoes %>% distinct(HostSpecies_SBMz, ParSpecies, Biome) #network links biome

int.biome <- interacoes %>%  filter(!is.na(Biome)) 
int.biome <- interacoes %>%  filter(Biome!="not_found") %>%
  distinct(HostSpecies_SBMz, ParSpecies, Biome) #network links Biome

host.biome <- interacoes %>% filter(!is.na(Biome)) %>%
  distinct(HostSpecies_SBMz, HostOrder, Biome) #network nodes Biome

par.biome <- interacoes %>% filter(!is.na(Biome)) %>%
  distinct(ParSpecies, ParGroup, Biome) #network nodes Biome

names(int.biome)[names(int.biome) == "HostSpecies_SBMz"] <- "Source"
names(int.biome)[names(int.biome) == "ParSpecies"] <- "Target"

names(par.biome)[names(par.biome) == "ParSpecies"] <- "id"
names(host.biome)[names(host.biome) == "HostSpecies_SBMz"] <- "id"

names(host.biome)[names(host.biome) == "HostOrder"] <- "grupo"
names(par.biome)[names(par.biome) == "ParGroup"] <- "grupo"
par.biome$label <- par.biome$id

nodes.biome <- bind_rows(host.biome, par.biome)

nodes.biome <- nodes.biome%>%distinct(grupo, id)

pampa <- int.biome %>% filter(Biome == "pampa")
pantanal <- int.biome %>% filter(Biome == "pantanal") 
amazon <- int.biome %>% filter(Biome == "amazon_rainforest")
atlantic <- int.biome %>% filter(Biome == "atlantic_forest")
cerrado <- int.biome %>% filter(Biome == "cerrado")
caatinga <- int.biome %>% filter(Biome == "caatinga")

pampa.net<-pampa%>%distinct(Source, Target)
#Create the igraph networks
net_ig <- graph_from_data_frame(d=pampa.net, v=NULL, directed=F) 
#Degree
deg.ig <- degree(net_ig, v = V(net_ig), loops = TRUE, normalized = FALSE)
#Closeness
clos.ig <- closeness(net_ig, vids = V(net_ig), weights = NULL, normalized = TRUE)
#Betweeness
btw.ig <- betweenness(net_ig, v = V(net_ig), directed = FALSE, weights = NULL, normalized = TRUE)

deg.dist <- as.data.frame(deg.ig)
clos.ig <- as.data.frame(clos.ig)
btw.ig <- as.data.frame(btw.ig)
pampa.net<-bind_cols(deg.dist, clos.ig, btw.ig)
pampa.net$nodes <- rownames(pampa.net)
pampa.net<- pampa.net%>%left_join(nodes.biome, by=c("nodes"="id"))

pantanal.net<-pantanal%>%distinct(Source, Target)
#Create the igraph networks
net_ig <- graph_from_data_frame(d=pantanal.net, v=NULL, directed=F) 
#Degree
deg.ig <- degree(net_ig, v = V(net_ig), loops = TRUE, normalized = FALSE)
#Closeness
clos.ig <- closeness(net_ig, vids = V(net_ig), weights = NULL, normalized = TRUE)
#Betweeness
btw.ig <- betweenness(net_ig, v = V(net_ig), directed = FALSE, weights = NULL, normalized = TRUE)

deg.dist <- as.data.frame(deg.ig)
clos.ig <- as.data.frame(clos.ig)
btw.ig <- as.data.frame(btw.ig)
pantanal.net<-bind_cols(deg.dist, clos.ig, btw.ig)
pantanal.net$nodes <- rownames(pantanal.net)
pantanal.net<- pantanal.net%>%left_join(nodes.biome, by=c("nodes"="id"))

amazon.net<-amazon%>%distinct(Source, Target)
#Create the igraph networks
net_ig <- graph_from_data_frame(d=amazon.net, v=NULL, directed=F) 
#Degree
deg.ig <- degree(net_ig, v = V(net_ig), loops = TRUE, normalized = FALSE)
#Closeness
clos.ig <- closeness(net_ig, vids = V(net_ig), weights = NULL, normalized = TRUE)
#Betweeness
btw.ig <- betweenness(net_ig, v = V(net_ig), directed = FALSE, weights = NULL, normalized = TRUE)

deg.dist <- as.data.frame(deg.ig)
clos.ig <- as.data.frame(clos.ig)
btw.ig <- as.data.frame(btw.ig)
amazon.net<-bind_cols(deg.dist, clos.ig, btw.ig)
amazon.net$nodes <- rownames(amazon.net)
amazon.net<- amazon.net%>%left_join(nodes.biome, by=c("nodes"="id"))

atlantic.net<-atlantic%>%distinct(Source, Target)
#Create the igraph networks
net_ig <- graph_from_data_frame(d=atlantic.net, v=NULL, directed=F) 
#Degree
deg.ig <- degree(net_ig, v = V(net_ig), loops = TRUE, normalized = FALSE)
#Closeness
clos.ig <- closeness(net_ig, vids = V(net_ig), weights = NULL, normalized = TRUE)
#Betweeness
btw.ig <- betweenness(net_ig, v = V(net_ig), directed = FALSE, weights = NULL, normalized = TRUE)

deg.dist <- as.data.frame(deg.ig)
clos.ig <- as.data.frame(clos.ig)
btw.ig <- as.data.frame(btw.ig)
atlantic.net<-bind_cols(deg.dist, clos.ig, btw.ig)
atlantic.net$nodes <- rownames(atlantic.net)
atlantic.net<- atlantic.net%>%left_join(nodes.biome, by=c("nodes"="id"))

cerrado.net<-cerrado%>%distinct(Source, Target)
#Create the igraph networks
net_ig <- graph_from_data_frame(d=cerrado.net, v=NULL, directed=F) 
#Degree
deg.ig <- degree(net_ig, v = V(net_ig), loops = TRUE, normalized = FALSE)
#Closeness
clos.ig <- closeness(net_ig, vids = V(net_ig), weights = NULL, normalized = TRUE)
#Betweeness
btw.ig <- betweenness(net_ig, v = V(net_ig), directed = FALSE, weights = NULL, normalized = TRUE)

deg.dist <- as.data.frame(deg.ig)
clos.ig <- as.data.frame(clos.ig)
btw.ig <- as.data.frame(btw.ig)
cerrado.net<-bind_cols(deg.dist, clos.ig, btw.ig)
cerrado.net$nodes <- rownames(cerrado.net)
cerrado.net<- cerrado.net%>%left_join(nodes.biome, by=c("nodes"="id"))

caatinga.net<-caatinga%>%distinct(Source, Target)
#Create the igraph networks
net_ig <- graph_from_data_frame(d=caatinga.net, v=NULL, directed=F) 
#Degree
deg.ig <- degree(net_ig, v = V(net_ig), loops = TRUE, normalized = FALSE)
#Closeness
clos.ig <- closeness(net_ig, vids = V(net_ig), weights = NULL, normalized = TRUE)
#Betweeness
btw.ig <- betweenness(net_ig, v = V(net_ig), directed = FALSE, weights = NULL, normalized = TRUE)

deg.dist <- as.data.frame(deg.ig)
clos.ig <- as.data.frame(clos.ig)
btw.ig <- as.data.frame(btw.ig)
caatinga.net<-bind_cols(deg.dist, clos.ig, btw.ig)
caatinga.net$nodes <- rownames(caatinga.net)
caatinga.net<- caatinga.net%>%left_join(nodes.biome, by=c("nodes"="id"))

write.xlsx(pampa.net, file = "NET_Biome_pampa_centralidades.xlsx")
write.xlsx(pantanal.net, file = "NET_Biome_pantanal_centralidades.xlsx")
write.xlsx(amazon.net, file = "NET_Biome_amazon_centralidades.xlsx")
write.xlsx(atlantic.net, file = "NET_Biome_atlantic_centralidades.xlsx")
write.xlsx(cerrado.net, file = "NET_Biome_cerrado_centralidades.xlsx")
write.xlsx(caatinga.net, file = "NET_Biome_caatinga_centralidades.xlsx")
