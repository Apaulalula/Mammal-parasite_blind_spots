# Figure INEXT
library(ggplot2)
library(dplyr)

# opcional: ordenar biomas (melhora narrativa)
ordem_biomas <- c(
  "amazon_rainforest",
  "atlantic_forest",
  "cerrado",
  "caatinga",
  "pantanal",
  "pampa"
)

curves_df <- results_group %>%
  unnest(curve) %>%
  mutate(group = paste(biome, parasite, sep = " | ")) %>% 
  mutate(biome = factor(biome, levels = ordem_biomas)) %>%
  mutate(biome = recode(biome,
                        "amazon_rainforest" = "Amazon",
                        "atlantic_forest" = "Atlantic Forest",
                        "cerrado" = "Cerrado",
                        "caatinga" = "Caatinga",
                        "pantanal" = "Pantanal",
                        "pampa" = "Pampa"
  ))

#plot -----

ggplot(curves_df, aes(x = t, y = qD, color = parasite)) +
  geom_line(aes(linetype = Method), size = 1) +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = parasite),
              alpha = 0.2, color = NA) +
  facet_wrap(~ biome, scales = "free") +
  labs(
    x = "Number of studies",
    y = "Parasite richness",
    color = "Parasite group",
    fill = "Parasite group",
    linetype = "Method"
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
    y = "Parasite richness"
  ) +
  theme_bw()


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
    fill = "Parasite group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

summary(results$parasite_richness)
hist(results$parasite_richness)

ggplot(data.frame(obs = y_test,
                  pred = pred),
       aes(obs, pred)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red")
