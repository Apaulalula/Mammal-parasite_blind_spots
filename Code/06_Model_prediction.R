# Packages
pacman::p_load(xgboost, caret, Matrix, fastDummies, performance)

# Load models
df_all <- read.csv2("./data/df_model.csv", sep =",") #all data

model_s <- load("./Output/model_s.RData")
model_ns <- load("./Output/model_ns.RData")

# change effort
df_all_highEffort <- df_all %>%
  mutate(n_study = 40) %>%  
  glimpse()

# train columns 
X_all <- df_all_highEffort %>%
  select(colnames(X_study)) %>%
  as.matrix()

# potential richness
# 1. Gerar as previsões do modelo para a base de teste
pred_test <- predict(model_ns, newdata = dtest)

pred_high <- predict(model_s, X_all)

# Calculate knowledge gap

results <- bmpo_ml %>%
  mutate(
    predicted_high_effort = pred_high,
    completeness =  parasite_richness / predicted_high_effort,
    RDD = (predicted_high_effort - parasite_richness) /
      predicted_high_effort)

# Priority species

priority_species <- results %>%
  arrange(desc(completeness)) %>%
  filter(completeness > 1) %>% 
  select(HostSpecies_SBMz, Parasite, BIOMAS, parasite_richness, predicted_high_effort, completeness, n_study)

unique(priority_species$HostSpecies_SBMz)

# Hotspot biomes

hotspots <- results %>%
  group_by(BIOMAS) %>%
  summarise(completeness = mean(completeness, na.rm = TRUE)) %>%
  arrange(desc(completeness))

# plot
ggplot(results, aes(x = parasite_richness, y = predicted_high_effort, color = n_studies)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    x = "Riqueza observada",
    y = "Riqueza prevista (alto esforço)"
  )

# gap vs effort
ggplot(results, aes(x = n_study, y = completeness)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  theme_minimal()

biome_rdd <- results %>%
  group_by(BIOMAS, Parasite) %>%
  dplyr::summarise(
    mean_RDD = mean(RDD, na.rm = TRUE),
    sd_RDD = sd(RDD, na.rm = TRUE),
    n_species = n()
  )

ggplot(biome_rdd, 
       aes(x = BIOMAS, y = mean_RDD, fill = Parasite)) +
  
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

ggplot(results %>%  filter(completeness <= 2) %>% 
         mutate(biome = factor(BIOMAS, levels = ordem_biomas)) %>%
         mutate(biome = recode(BIOMAS,
                               "amazon_rainforest" = "Amazon",
                               "atlantic_forest" = "Atlantic Forest",
                               "cerrado" = "Cerrado",
                               "caatinga" = "Caatinga",
                               "pantanal" = "Pantanal",
                               "pampa" = "Pampa")),
       aes(x = biome, y = completeness, fill = Parasite)) +
  geom_boxplot(alpha= 0.5) +
  #geom_jitter(aes(color = Parasite), alpha = 0.3)+
  scale_fill_manual(values = c(
    "Macro" = "#D55E00",
    "Micro" = "#0072B2"
  )) +
  labs(
    x = NULL,
    y = "Completeness",
    fill = "Parasite type"
  ) +
  facet_wrap(~threat_score)+
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
