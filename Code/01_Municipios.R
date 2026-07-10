#-------------------------------------------------------------------------------
# PROJECT: Predicting Mammal's parasite richness in Brazil Biomes
#
# OBJECTIVE: 
#
# PRODUCT: Clean datasets
#
# AUTHOR: Ana Paula Lula Costa
#
# Criation date: 01/2026
#
# Modification date: mm/aaaa
#
#-------------------------------------------------------------------------------

#Packages---------------------------------

library(sf)
library(tidyverse)

#Load shapefiles------------

bmpo <- readxl::read_xlsx("./raw_data/data06_BMPO_Data.xlsx")

municipios <- st_read("./shapefiles/BR_Municipios_2024/BR_Municipios_2024.shp")

biomas <- st_read("./shapefiles/dmRgNtTrBmBr_v2025_dominios_vetor_e250K.shp")

biomas <- st_transform(biomas, st_crs(municipios))

# Inspect data ------------------------------

colnames(bmpo)

# filter relevant data

bmpo_filtered <- bmpo[, c(1:9, 31:45, 59:67)] %>% 
  filter(
    ParStatus %in% c("species", "subspecies", "serotype", "biotype", "varietas")
  ) %>% 
  filter(Domesticated_or_Wild == "Wild") %>% 
  select(HostGroup, HostOrder, HostSpecies_SBMz, ParasiteCurrentName, ParGroup,MunicipalityCode, Biome, ReferenceUsedParasite) %>%
  filter(ParGroup != "not_found",
         HostSpecies_SBMz != "not_found") %>%
  mutate(Parasite = case_when(
    ParGroup %in% c("Fungi","Protozoa", "Bacteria","RNA_virus", "DNA_virus", "Virus") ~ "Micro", 
    ParGroup %in% c("Helminth", "Arthropod") ~ "Macro")) %>% 
  group_by(HostOrder) %>% 
  mutate(
    HostGroup = ifelse(
      is.na(HostGroup) & n_distinct(HostOrder, na.rm = TRUE) == 1,
      unique(na.omit(HostOrder)),
      HostGroup
    )
  ) %>%
  ungroup() %>% 
  glimpse()

unique(bmpo_filtered$HostGroup)

# Integrate municipality and biome shapefiles -------

mun_bioma <- st_intersection(municipios, biomas)

# Group by municipality and filter biomes
mun_bioma <- mun_bioma %>%
  mutate(area = st_area(.)) %>%
  group_by(CD_MUN) %>%   # código IBGE do município
  slice_max(area, n = 1) %>%
  ungroup() %>% 
  glimpse()

# Dataset with biome list by municipality

M_biome_select<- mun_bioma %>% 
  as.data.frame() %>% 
  select(MunicipalityCode, nm_bm) %>% 
  rename("MunicipalityCode" = CD_MUN) %>% 
  group_by(MunicipalityCode, nm_bm) %>% 
  summarise(Count= 1, .groups = 'drop') %>%
  pivot_wider(
    names_from = nm_bm,
    values_from = Count,
    values_fill = 0
   ) %>%   
  pivot_longer(
    cols = Amazônia:Pantanal,
    names_to = "Biome_new",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%
  group_by(across(-c(Biome_new, value))) %>%
  summarise(Biome_new = str_c(Biome_new, collapse = "_"), .groups = "drop")

# Integrate BMPO and Biome data

bmpo_biome_new <- bmpo_filtered %>% 
  mutate(MunicipalityCode = as.integer(MunicipalityCode)) %>% 
  left_join(M_biome_select) %>% 
  mutate(BIOMAS = case_when(
    Biome_new =is.na(Biome_new) ~ Biome, 
    .default = as.character(Biome_new)
    )
    )

glimpse(bmpo_biome_new)

# Change biome names

bmpo_biome_final<- bmpo_biome_new %>%
  mutate(
    BIOMAS = recode(BIOMAS,
      "Mata.Atlântica" = "atlantic_forest",
      "Cerrado_Mata.Atlântica" = "atlantic_forest.cerrado",
      "Cerrado" = "cerrado",
      "Amazônia_Cerrado" = "amazon_rainforest.cerrado",
      "Caatinga" = "caatinga",
      "Cerrado_Caatinga" = "cerrado.caatinga",
      "Mata.Atlântica_Pampa" = "atlantic_forest",
      "Caatinga_Mata.Atlântica" = "caatinga.atlantic_forest",
      "Cerrado_Pantanal" = "cerrado.pantanal",
      "Pampa" = "pampa",
      "Amazônia_Pantanal" = "amazon_rainforest.pantanal",
      "Amazônia" = "amazon_rainforest",
      "Amazônia_Cerrado_Pantanal" = "amazon_rainforest.cerrado",
      "Mata.Atlântica_Ilhas.Oceânicas" = "atlantic_forest",
      "Ilhas.Oceânicas" = "ocean_islands",
      "Cerrado_Caatinga_Mata.Atlântica" = "atlantic_forest.cerrado",
      "Pantanal" = "pantanal",
      "caatinga_or_atlantic_forest" = "caatinga.atlantic_forest"
    ),
    BIOMAS = recode(BIOMAS,
      "transition_cerrado,_caatinga_and_atlantic_forest"= "atlantic_forest.cerrado"
    )
  ) %>% 
  separate_rows(BIOMAS, sep = "\\.") %>% 
  select(HostSpecies_SBMz, ParasiteCurrentName, ReferenceUsedParasite, Parasite, BIOMAS)

unique(bmpo_biome_final$BIOMAS)

Biome_bmpo <- bmpo_biome_final %>%
  select(HostSpecies_SBMz, BIOMAS) %>% 
  filter(BIOMAS != "not_found") %>%
  distinct()

# save data with biome information into bmpo dataset
write.csv(M_biome_select, "./data/mcp_biome.csv")

write.csv(bmpo_biome_new, "./data/Bmpo_Biome_all.csv", row.names = FALSE)
