# Load packages ----

pacman::p_load(tidyverse, vegan, ape, readxl)

# Load data ----

Host_data <- readxl::read_xlsx("./raw_data/Mammalia_SBMz_v2025-1.xlsx") %>% 
  dplyr::mutate(HostSpecies_SBMz = str_to_lower(BINOMIO),
                HostSpecies_SBMz = str_replace_all(HostSpecies_SBMz, " ", "_")) %>% 
  glimpse()

host_sinantropy <- read.csv2("./Data/host_sinantropy.csv", sep = ",") 

tax_axes <- read.csv2("./Data/tax.axes.csv", sep = ",")

# Work on host functional features to include in the XGboost model ----

## Diet proportion ----
bmpo_diet <- bmpo[,c(5,18:27)] %>% 
  group_by(HostSpecies_SBMz) %>% 
  summarise_all(mean) %>% 
  na.omit()

diet_cols <- grep("^Diet\\.", names(bmpo_diet), value = TRUE)

diet_matrix <- bmpo_diet[, diet_cols]

diet_prop <- diet_matrix / 100

# Calculate diet diversity
bmpo_diet$diet_shannon <- diversity(
  diet_prop,
  index = "shannon"
)

# Calculate diet distance matrix
diet_dist <- vegdist(
  diet_prop,
  method = "bray"
)

# Pcoa from diet distance matrix to build axis of diet similarity
diet_pcoa <- cmdscale(
  diet_dist,
  eig = TRUE,
  k = 2
)

eig <- diet_pcoa$eig

var_exp <- 100 * eig[eig > 0] / sum(eig[eig > 0])

var_exp[1:5] # analyse percentage of explication of the first 5 axis

bmpo_diet$diet_axis1 <- diet_pcoa$points[,1]
bmpo_diet$diet_axis2 <- diet_pcoa$points[,2]

Host_diet <- bmpo_diet[,c(1, 12:14)]

# visualize Pcoa diet representation

fit <- envfit(
  diet_pcoa$points,
  diet_prop,
  permutations = 999
)

fit

plot(
  diet_pcoa$points,
  pch = 19,
  col = "grey50",
  xlab = "Diet Axis 1",
  ylab = "Diet Axis 2"
)

plot(
  fit,
  p.max = 0.05,
  col = "red"
)

## Work on extinction vulnerability categories ----

unique(Host_data$`MMA 2023`)
unique(bmpo$IUCN.Status)

bmpo_threat <- bmpo %>% 
  select(HostSpecies_SBMz, IUCN.Status) %>% 
  distinct() %>% 
  filter(HostSpecies_SBMz != "not_found") %>% 
  mutate(threatened = case_when(
    IUCN.Status %in% c("VU", "EN", "CR", "DD") ~ 1,
    IUCN.Status %in% c("LC", "NT") ~ 0,
    TRUE ~ NA_real_
  ))

Host_threat <- Host_data %>% 
  select(HostSpecies_SBMz, `MMA 2023`) %>% 
  rename(IUCN.Status = `MMA 2023`) %>% 
  mutate(threatened = case_when(
    IUCN.Status %in% c("VU", "EN", "CR", "DD") ~ 1,
    IUCN.Status %in% c("LC", "NT") ~ 0,
  TRUE ~ NA_real_
)) %>% 
  bind_rows(anti_join(bmpo_threat, Host_data, by = "HostSpecies_SBMz"))

table(Host_threat$threatened, useNA = "always")

# Make a score representing a gradient where a higher value represents a more threatened species and a lower value a least concern species
# we chose to include species categorize as Insufficient data (DD) as a mid concern value (1), thinking that few information is a critical stage in conservation

Host_threat$threat_score <- dplyr::case_when(
  Host_threat$IUCN.Status == "LC" ~ 0,
  Host_threat$IUCN.Status == "DD" ~ 1,
  Host_threat$IUCN.Status == "NT" ~ 2,
  Host_threat$IUCN.Status == "VU" ~ 3,
  Host_threat$IUCN.Status == "EN" ~ 4,
  Host_threat$IUCN.Status == "CR" ~ 5,
  TRUE ~ NA_real_
)

## Work on biome occurrence data ----
# Biome occurrence based on SBMz data. This information will be used to fill biome missing information in bmpo

bioma_sbmz <- Host_data %>%
  select(HostSpecies_SBMz, `OCORRENCIA BIOMAS`) %>% 
  mutate(
    BIOMAS = `OCORRENCIA BIOMAS`,
    BIOMAS = str_replace_all(BIOMAS, "Amazonia", "Amazônia"),
    BIOMAS = str_replace_all(BIOMAS, "Mata Atântica", "Mata Atlântica"),
    BIOMAS = str_replace_all(BIOMAS, "Cerrrado", "Cerrado"),
    BIOMAS = str_replace_all(BIOMAS, "Caatiga", "Caatinga"),
    BIOMAS = str_replace_all(BIOMAS, "caatinga", "Caatinga"),
    BIOMAS = str_replace_all(BIOMAS, " e ", ", ")
  ) %>%
  separate_rows(BIOMAS, sep = ",\\s*") %>%
  mutate(
    BIOMAS = case_when(
      BIOMAS == "Amazônia" ~ "amazon_rainforest",
      BIOMAS == "Caatinga" ~ "caatinga",
      BIOMAS == "Cerrado" ~ "cerrado",
      BIOMAS == "Mata Atlântica" ~ "atlantic_forest",
      BIOMAS == "Pampa" ~ "pampa",
      BIOMAS == "Pantanal" ~ "pantanal",
      BIOMAS %in% c(
        "Sistema Costeiro-Marinho",
        "Sistema Costeiro-Estuarino",
        "Marinho Nerítico",
        "Marinho Oceânico",
        "Sistema Marinho Nerítico",
        "Sistema Marinho Oceânico",
        "Sistema Costeiro",
        "Estuarino"
      ) ~ "marine",
      TRUE ~ BIOMAS
    )
 ) %>%
  select(-`OCORRENCIA BIOMAS`)

# Filling biome information of species lacking in BMPO

Biome_all <- bind_rows(
  Biome_bmpo,
  anti_join(bioma_sbmz, Biome_bmpo, by = "HostSpecies_SBMz")
)

unique(Biome_all$BIOMAS)

Biome_all <- Biome_all %>% 
  filter(!BIOMAS %in% c("ocean_islands", "marine"))

# Gather all features in one dataframe ----

Host_traits <- Host_data %>%
  dplyr::full_join(tax_axes, by = "HostSpecies_SBMz") %>%
  dplyr::full_join(Host_threat, by = "HostSpecies_SBMz") %>% 
  dplyr::full_join(Host_diet, by = "HostSpecies_SBMz") %>% 
  dplyr::full_join(host_sinantropy, by = "HostSpecies_SBMz") %>% 
  dplyr::full_join(Biome_all, by = "HostSpecies_SBMz") %>% 
  dplyr::select(
    HostSpecies_SBMz, diet_shannon, diet_axis1, diet_axis2, sinantropia,
    threatened, threat_score, TaxoPC1, TaxoPC2, BIOMAS
  ) %>% 
  filter(HostSpecies_SBMz %in% m_species)

Host_traits$sinantropia[is.na(Host_traits$sinantropia)] <- 0

write.csv(Host_traits, "./data/Host_traits.csv", row.names = FALSE)
