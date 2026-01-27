#Integrating databases for spatial analysis of parasite-host 
#associations and the novel Brazilian dataset 
#Cruz et al

#Funding: MCTI/CNPq Sinbiose.

## Extraindo tabela Country Organisms Interactions da pagina do EID2

## Rodei a funcao duas vezes, na primeira segui os passos abaixo, 
#porem adicionando o filtro Cargo_Taxa == species

## Na segunda, inclui os Cargos classificados como "no rank"
## o numero das paginas dos registros para Cargo_Taxa == no rank corresponde a segunda rodada

## Adicionados os filtros na pagina da web: 
#Carrier_Rank != no rank
#Carrier_Taxa != plants
#Carrier != homo sapiens
#Cargo_Taxa != plants
#Country = Brazil


## Adicionados os filtros na tabela extraida:
#Carrier_Rank == species & Carrier_Rank == subspecies

# load library
require(rvest)
require(tidyverse)


# Extraindo dados de tabela dinamica... 

# Note que a url vai mudando de acordo com a pag
pag_2 <- "https://eid2.liverpool.ac.uk/TripleRelationInteractions?Grid-page=2&Grid-orderBy=~&Grid-filter=Carrier~ne~%27homo%20sapiens%27~and~CarrierNCBIRank~ne~%27no%20rank%27~and~CarrierNCBIDivision~ne~%27plants%27~and~CargoNCBIRank~eq~%27species%27~and~CargoNCBIDivision~ne~%27plants%27~and~GeoCountry~eq~%27brazil%27"
pag_8 <- "https://eid2.liverpool.ac.uk/TripleRelationInteractions?Grid-page=8&Grid-orderBy=~&Grid-filter=Carrier~ne~%27homo%20sapiens%27~and~CarrierNCBIRank~ne~%27no%20rank%27~and~CarrierNCBIDivision~ne~%27plants%27~and~CargoNCBIRank~eq~%27species%27~and~CargoNCBIDivision~ne~%27plants%27~and~GeoCountry~eq~%27brazil%27"


pag_2 == pag_8


# Um jeito de resolver isso e especificando as paginas que voce quer e colocar isso dentro de uma funcao

scrap_pages <- function(x) {
  x <- as.character(x)
  url_final<- paste0("https://eid2.liverpool.ac.uk/TripleRelationInteractions?Grid-page=",x,"&Grid-orderBy=~&Grid-filter=Carrier~ne~%27homo%20sapiens%27~and~CarrierNCBIDivision~ne~%27plants%27~and~CargoNCBIDivision~ne~%27plants%27~and~GeoCountry~eq~%27brazil%27")
  
  page <- read_html(url_final)
  
  # Extraindo os nodes
  tab <- page %>% 
    html_nodes("table")
  
  
  # Essa tabela tava esquisita, entao eu dei uma inspecionada vizual e vi que os dados estao no node 2
  
  tab_ext <- tab[[2]] %>%
    html_table
  
  
  # Colocando os nomes
  # Os nomes tambem estao armazenados na tag "table"
  
  tab_names <- tab[[1]] %>%
    html_table %>% 
    names %>%
    str_remove_all("Filter") %>%
    str_replace_all("\\s", "_")
  
  
  ## Juntando tudo:
  
  tab_final <- tab_ext %>% set_names(tab_names) %>%
    mutate(page_number = x)
  
  return(as.data.frame(tab_final))
  
  
  
}


# Agora voce pode rodar a funcao pra todas as paginas que quiser:
test <- scrap_pages(1)

test %>% view

# usando o Map:

npag = 1:12 # O numero das paginas que voce estiver interessada
tabelas <- map_df(npag,scrap_pages)

tabelas %>% view


tabelas$original_database = "EID2"

tabelas <- tabelas %>% 
  filter(Carrier_Rank %in% c("species", "subspecies")&
           Carrier_Taxa != "invertebrates"&
           Cargo_Taxa != "environmental samples")

write.csv(tabelas, file = "EID2_OUT18.csv")


