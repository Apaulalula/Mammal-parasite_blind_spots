# Packages
pacman::p_load(tidyverse, ggplot2, caret, devtools, viridis, patchwork, hrbrthemes, fmsb, colormap)

# Load models
df_all <- read.csv2("./data/df_model.csv", sep =",") #all data

model_s <- readRDS("./Output/model_s.model")
model_ns <- readRDS("./Output/model_ns.model")

#Questions to be answered:
#(i) What Brazilian biomes concentrate the highest knowledge gaps? 
#(ii) Do macro and microparasites have distinct study patterns? 
#(iii) Are biodiversity hotspots also knowledge gap hotspots? 
# (iv) Which mammal species are the ones more and less studied? 
#(v) Are more studied species the ones more adapted to anthropogenic habitats?
  

# Making predictions ----
# Changing effort
df_all_highEffort <- df_all %>%
  mutate(n_study = 40) %>%  
  glimpse()

# Train columns for both models
X_all <- df_all_highEffort %>%
  select(- c(HostSpecies_SBMz, parasite_richness)) %>%
  mutate_all(as.numeric) %>%
  as.matrix()

X_all_ns <- df_all_highEffort %>%
  select(- c(HostSpecies_SBMz, parasite_richness, n_study)) %>%
  mutate_all(as.numeric) %>%
  as.matrix()

# Calculating potential richness predict by model with and without n_study
# generate prediction in model with test data
pred_test <- predict(model_ns, newdata = X_all_ns)

# generate prediction in model with all data but same number of studies
pred_high <- predict(model_s, X_all)

# Calculating knowledge gaps ----
Host_order <- taxa_all %>%
  select(HostSpecies_SBMz, HostOrder)

results <- bmpo_ml %>%
  mutate(
    predicted_without_nstudy = pred_test,
    predicted_high_effort = pred_high,
    completeness =  parasite_richness / predicted_high_effort,
    RDD = (predicted_high_effort - parasite_richness) /
      predicted_high_effort) %>% #relative diversity deficit
  left_join(Host_order)

species_studied <- unique(results$HostSpecies_SBMz)

# Looking at species with no study reported in bmpo ----
no_study_species <- Host_order %>% 
  inner_join(Biome_all, by = "HostSpecies_SBMz") %>% 
  filter(!HostSpecies_SBMz %in% species_studied) %>% 
  mutate(
    completeness = 0,
    RDD          = 1,
    n_study      = 0
  )

no_doc_n<- ggplot(no_study_species %>% 
       group_by(HostSpecies_SBMz) %>% 
         summarise(n_biomes = n()), aes(x= as.factor(n_biomes)))+
  geom_bar(aes(fill = n_biomes)) +
  labs(title= "Species with no documented interaction on BMPO dataset",
       x= "Number of biomes",
       y= "Number of mammal species")+
  theme_projeto(angle= 360, legend = "none")

ggsave(
  filename = "./Figures/no_doc_n.png",
  plot = no_doc_n,
  width = 6,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

no_doc_biome<- ggplot(no_study_species %>% 
         mutate(biome = factor(BIOMAS, levels = ordem_biomas),
                                   biome = recode(BIOMAS,
                                                  "amazon_rainforest" = "Amazon Rainforest",
                                                  "atlantic_forest" = "Atlantic Forest",
                                                  "cerrado" = "Cerrado",
                                                  "caatinga" = "Caatinga",
                                                  "pantanal" = "Pantanal",
                                                  "pampa" = "Pampa")), aes(x= biome))+
  geom_bar() +
  labs(title= "Species with no documented interaction on BMPO dataset",
       x= "",
       y= "Number of mammal species")+
  theme_projeto(legend = "none")

ggsave(
  filename = "./Figures/no_doc_biome.png",
  plot = no_doc_biome,
  width = 6,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Relationship with predicted and observaed richeness ----
# linear and R²
r2_labels <- results %>%
  mutate(Parasite = recode(Parasite,
                           "Micro" = "Microparasites",
                           "Macro" = "Macroparasites"
  )) %>% 
  group_by(Parasite) %>%
  summarise(
    r2 = summary(lm(predicted_high_effort ~ parasite_richness))$r.squared,
    x = Inf,
    y = -Inf,
    label = paste0("italic(R)^2 == ", round(r2, 3)),
    .groups = "drop"
  )

hist(results$n_study)

p_obs_vs <- ggplot(results %>% 
                     mutate(Parasite = recode(Parasite,
                                              "Micro" = "Microparasites",
                                              "Macro" = "Macroparasites"
                                              )),
                   aes(x = parasite_richness,
                       y = predicted_high_effort,
                       color = n_study)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "red") +
  geom_text(
    data = r2_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    parse = TRUE,
    hjust = 1.1,
    vjust = -0.5,
    size = 4.5
  ) +
  scale_color_viridis_c(
    option = "turbo",
    name = "Study effort"
  ) +
  facet_wrap(~Parasite) +
  theme_projeto(angle= 360)+
  labs(
    x = "Observed richness",
    y = "Predicted (high study effort)"
  )

ggsave(
  filename = "./Figures/Observed_vs_Predicted_HighStudyEffort.png",
  plot = p_obs_vs,
  width = 12,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Relationship between prediction with and without n_study ----
r2_labels <- results %>% 
  mutate(Parasite = recode(Parasite,
                           "Micro" = "Microparasites",
                           "Macro" = "Macroparasites"
  )) %>% 
  group_by(Parasite) %>%
  summarise(
    r2 = summary(lm(predicted_without_nstudy ~ parasite_richness))$r.squared,
    x = Inf,
    y = -Inf,
    label = paste0("italic(R)^2 == ", round(r2, 3)),
    .groups = "drop"
  )

p_obs_vs_nostudy <- ggplot(results %>% 
                             mutate(Parasite = recode(Parasite,
                                                      "Micro" = "Microparasites",
                                                      "Macro" = "Macroparasites"
                                                      )),
                   aes(x = parasite_richness,
                       y = predicted_without_nstudy,
                       color = n_study)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "red") +
  geom_text(
    data = r2_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    parse = TRUE,
    hjust = 1.1,
    vjust = -0.5,
    size = 4.5
  ) +
  scale_color_viridis_c(
    option = "turbo",
    name = "Study effort"
  ) +
  facet_wrap(~Parasite) +
  theme_projeto(angle= 360)+
  labs(
    x = "Observed richness",
    y = "Predicted (no study effort)"
  )

ggsave(
  filename = "./Figures/Observed_vs_Predicted_NoStudyEffort.png",
  plot = p_obs_vs_nostudy,
  width = 12,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Relationship between high effort vs no study ----
high_no<- ggplot(results, aes(x = predicted_without_nstudy, y = predicted_high_effort, color = n_studies)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_projeto(angle= 360, legend= "none")+
  labs(
    x = "Predicted (no study effort)",
    y = "Predicted (high study effort)"
  )

ggsave(
  filename = "./Figures/High_NoStudyEffort.png",
  plot = high_no,
  width = 6,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Seeing which mammals are the ones more and less knowledge gaps ----
more_study_species <- results %>%
  arrange(desc(completeness)) %>%
  filter(completeness > 1) %>% 
  select(HostSpecies_SBMz, HostOrder, Parasite, BIOMAS, parasite_richness, predicted_high_effort, completeness, n_study)

less_study_species <- all_results %>%
  arrange(desc(completeness)) %>%
  filter(completeness < 0.5) %>% 
  select(HostSpecies_SBMz, HostOrder, Parasite, BIOMAS, parasite_richness, predicted_high_effort, completeness, n_study)

species_biome <- biome_plot(results)

ggsave(
  filename = "./Figures/Species_per_biome.png",
  plot = species_biome,
  width = 10,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Seeing relationship between biomes and highest knowledge gaps ----

# Completeness vs effort by number of biome the species occur in

plot_n_biome<- Completeness_nstudy_plot(results, title = "")

ggsave(
  filename = "./Figures/plot_n_biome.png",
  plot = plot_n_biome,
  width = 12,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Plot mean and distribution of completeness per biome and parasites

mean_biome<- results %>%
  mutate(biome = factor(BIOMAS, levels = ordem_biomas),
         biome = recode(BIOMAS,
                        "amazon_rainforest" = "Amazon Rainforest",
                        "atlantic_forest" = "Atlantic Forest",
                        "cerrado" = "Cerrado",
                        "caatinga" = "Caatinga",
                        "pantanal" = "Pantanal",
                        "pampa" = "Pampa")) %>% 
  group_by(biome, Parasite) %>%
  summarise(media = mean(completeness, na.rm = TRUE), .groups = "drop") %>%
  # Define vjust diferente para cada nível de Parasite (evita sobreposição vertical)
  group_by(biome) %>%
  mutate(
    pos_vjust = ifelse(row_number() == 1, 1.5, 3.2)
  ) %>%
  ungroup()

dens_plot<- ggplot(results, aes(x = completeness)) +
  # Histograma em proporção de densidade (position = "identity" evita empilhamento confuso)
  geom_histogram(
    aes(y = after_stat(density), fill = Parasite),
    position = "identity",
    bins = 30,
    alpha = 0.35,
    color = "white"
  ) +
  # Curvas de densidade por Parasite
  geom_density(aes(color = Parasite), linewidth = 1) +
  # Linhas verticais coloridas por Parasite
  geom_vline(
    data = mean_biome,
    aes(xintercept = media, color = Parasite),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  # Rótulos com o valor da média empilhados verticalmente
  geom_text(
    data = mean_biome,
    aes(
      x = media,
      y = Inf,
      label = paste0(Parasite, ": ", round(media, 2)),
      color = Parasite,
      vjust = pos_vjust
    ),
    hjust = -0.08,
    size = 3.2,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_color_parasite()+
  facet_wrap(~ biome, scales = "fixed", nrow = 3,
             ncol = 2) +
  # Usa results em vez de dados para o limite
  scale_x_continuous(limits = c(0, quantile(results$completeness, 0.98, na.rm = TRUE))) +
  labs(
    x = "Completeness",
    y = "Density"
  ) +
  theme_projeto(angle= 360, legend= "none")

ggsave(
  filename = "./Figures/dens_biome.png",
  plot = dens_plot,
  width = 10,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)
