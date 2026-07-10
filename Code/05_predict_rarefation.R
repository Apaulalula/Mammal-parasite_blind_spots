#-------------------------------------------------------------------------------
# PROJECT: Predicting Mammal's parasite richness in Brazil Biomes
#
# OBJECTIVE: 
#
# PRODUCT: Clean dataset bmpo
#
# AUTHOR: Ana Paula Lula Costa
#
# Criation date: 01/2026
#
# Modification date: mm/aaaa
#
#-------------------------------------------------------------------------------
#load packages
library(tidyverse)
library(ggplot2)
library(iNEXT)
library(vegan)

# load data

Bioma_final <- load("./data/Biome_final.rds")

# Inspect dataframes

glimpse(bmpo_filtered)
glimpse(Host_traits)


# Harmonize dataframes

bmpo_rarefaction <- Host_traits %>% 
  select(HostSpecies_SBMz, amazon_rainforest, cerrado, atlantic_forest, pantanal, pampa, caatinga, marine) %>% 
  right_join(bmpo_biome_new) %>% 
  unite("Interaction", HostSpecies_SBMz, ParasiteCurrentName) %>%   
  filter(Biome_new != "ocean_islands") %>%  
  pivot_longer(amazon_rainforest:marine, names_to = "Biome_SBMz", values_to = "Occurrence") %>% 
  filter(Occurrence == 1) %>% 
  select(- Occurrence) %>%
  group_by(Interaction, HostGroup, ParGroup, Parasite, Biome_new, ReferenceUsedParasite) %>%
  mutate(
    Biome_new = ifelse(
      Biome_new == "not_found",
      unique(na.omit(Biome_SBMz)),
      Biome_new
    )
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  select(- n)

glimpse(bmpo_rarefaction)
unique(bmpo_rarefaction$Biome_new)

write.csv(bmpo_biome_rarefaction, "./data/bmpo_biome_rarefaction.csv")

# predict with iNEXT

biome <- unique(bmpo_rarefaction$Biome_new)
parasite <- unique(bmpo_rarefaction$Parasite)

results_group <- Map(function(b, p){
  
  biome_data <- bmpo_rarefaction %>%
    filter(Biome_new == b,
           Parasite == p) %>%
    distinct(Interaction, ReferenceUsedParasite) %>%   # remove duplicatas
    filter(!is.na(ReferenceUsedParasite), !is.na(Interaction))
  
  mat <- biome_data %>% 
    select(ReferenceUsedParasite, Interaction) %>% 
    group_by(ReferenceUsedParasite, Interaction) %>%
    summarise(Occur= n(), .groups = 'drop') %>%
    pivot_wider(names_from= ReferenceUsedParasite, 
                values_from= Occur, 
                values_fill = 0) %>% 
    column_to_rownames(var= "Interaction") %>% 
    remove_rownames()
  
    inc_freq <- c(ncol(mat), rowSums(mat))
  
    out <- iNEXT(inc_freq, datatype = "incidence_freq", nboot = 100)
    est <- out$AsyEst
    
    return(data.frame(
      biome = b,
      parasite = p,
      n_studies = n_distinct(biome_data$ReferenceUsedParasite),
      richness_obs = est$Observed[1],
      richness_est = est$Estimator[1],
      se = est$Est_s.e.[1],
      note = "estimado"
    ))
}, biome, parasite)

final_rarefaction <- bind_rows(results_group)

# criar todas as combinações
comb <- expand.grid(
  biome = unique(bmpo_rarefaction$Biome_new),
  parasite = unique(bmpo_rarefaction$Parasite),
  stringsAsFactors = FALSE
)

results_group <- comb %>%
  pmap_dfr(function(biome, parasite) {
    
    biome_data <- bmpo_rarefaction %>%
      filter(Biome_new == biome,
             Parasite == parasite) %>%
      distinct(Interaction, ReferenceUsedParasite) %>%
      filter(!is.na(ReferenceUsedParasite), !is.na(Interaction))
    
    # evita erro com subconjunto vazio
    if (nrow(biome_data) == 0) return(NULL)
    
    mat <- biome_data %>% 
      count(ReferenceUsedParasite, Interaction, name = "Occur") %>%
      pivot_wider(names_from = ReferenceUsedParasite, 
                  values_from = Occur, 
                  values_fill = 0) %>% 
      column_to_rownames("Interaction")
    
    # garante que há pelo menos 1 coluna e 1 linha
    if (nrow(mat) == 0 || ncol(mat) == 0) return(NULL)
    
    inc_freq <- c(ncol(mat), rowSums(mat))
    
    out <- iNEXT(inc_freq, datatype = "incidence_freq", nboot = 100)
    est <- out$AsyEst
    est_m <- out[["iNextEst"]]$size_based
    
    name <- paste0(biome,"_", parasite)
  
    tibble(
      biome = biome,
      parasite = parasite,
      n_studies = n_distinct(biome_data$ReferenceUsedParasite),
      richness_obs = est$Observed[1],
      richness_est = est$Estimator[1],
      se = est$Est_s.e.[1],
      note = "estimado",
      curve = list(est_m) 
    )
  })

results_group <- results_group %>%
  mutate(
    deficit_rel = (richness_est - richness_obs) / richness_est
  )

#plot -----

curves_df <- results_group %>%
  unnest(curve) %>%
  mutate(group = paste(biome, parasite, sep = " | "))

ggplot(curves_df, aes(x = t, y = qD, color = parasite)) +
  
  # linhas das curvas
  geom_line(aes(linetype = Method), size = 1) +
  
  # intervalo de confiança (se disponível)
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = parasite),
              alpha = 0.2, color = NA) +
  
  # facet por bioma
  facet_wrap(~ biome, scales = "free") +
  
  labs(
    x = "Número de amostras (estudos)",
    y = "Riqueza de parasitos",
    color = "Tipo de parasito",
    fill = "Tipo de parasito",
    linetype = "Método"
  ) +
  
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


ggplot(curves_df, aes(x = SC, y = qD, color = parasite)) +
  geom_line(aes(linetype = Method), size = 1) +
  facet_wrap(~ biome, scale = "free") +
  labs(
    x = "Sample coverage",
    y = "Riqueza padronizada"
  ) +
  theme_bw()

curves_df %>%
  filter(Method != "observed") %>%
  ggplot(aes(x = t, y = qD, color = parasite)) +
  geom_line(size = 1) +
  facet_wrap(~ biome, scale = "free")


ggplot(results_group, 
       aes(x = biome, y = deficit_rel, fill = parasite)) +
  
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  scale_fill_manual(values = c(
    "Macro" = "#D55E00",
    "Micro" = "#0072B2"
  )) +
  
  labs(
    x = NULL,
    y = "Relative diversity deficit",
    fill = "Parasite type"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
