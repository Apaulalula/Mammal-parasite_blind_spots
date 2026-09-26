# Instale se não tiver: install.packages("fastDummies")

pacman::p_load(tidyverse, vegan, fastDummies, readxl)

# Habitat IUCN

sp_habitat <- read.csv("./Raw_data/IUCN_results_all_400_spp.csv")

# Dividindo a coluna com base no separador ";;"
dados_finais <- dummy_cols(sp_habitat, 
                           select_columns = "habitats", 
                           split = ";;", 
                           remove_selected_columns = TRUE)
 colnames(dados_finais)
 
 dados_finais <- dados_finais %>%
   mutate(
     sinantropia = case_when(
       # Se o habitat for explicitamente NA ou Desconhecido, mantém como NA
       (habitats_NA == 1 | habitats_Unknown == 1) ~ 0,
       
       # Se ocorrer em qualquer artificial, é sinantrópica
       if_any(starts_with("habitats_Artificial"), ~ .x == 1) ~ 1,
       
       # Caso contrário, florestal
       TRUE ~ 0
     )
   )
 
 
 host_sinantropy <- dados_finais %>% 
   select(species_used, sinantropia) %>%
   dplyr::mutate(HostSpecies_SBMz = str_to_lower(species_used),
                 HostSpecies_SBMz = str_replace_all(HostSpecies_SBMz, " ", "_")) %>% 
   select(-species_used) %>% 
   distinct() %>% 
   glimpse()

 
 write.csv(host_sinantropy, "./Data/host_sinantropy.csv", row.names = FALSE) 
 