#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

# load library
library("helminthR")


BrazilHostPars <- findLocation(location="Brazil", 
                               citation=TRUE, 
                               hostState=1) #hostState=1 eh o argumento para "in the wild"

write.csv(BrazilHostPars, file="London_Museum_Brazil.csv")




