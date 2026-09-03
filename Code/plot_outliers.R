library(ggplot2)
library(forcats)
library(dplyr)
library(ggrepel) # Pacote essencial para não sobrepor os textos dos outliers

outliers <- all_results %>%  
  mutate(HostOrder = fct_reorder(HostOrder, completeness, .fun = median, .desc = FALSE),
         biome = factor(BIOMAS, levels = ordem_biomas)) %>%
  mutate(biome = recode(BIOMAS,
                        "amazon_rainforest" = "Amazon",
                        "atlantic_forest" = "Atlantic Forest",
                        "cerrado" = "Cerrado",
                        "caatinga" = "Caatinga",
                        "pantanal" = "Pantanal",
                        "pampa" = "Pampa")) %>% 
  group_by(HostSpecies_SBMz, HostOrder, biome) %>%
  dplyr::summarise(
    mean_cp = mean(completeness, na.rm = TRUE),
    sd_cp = sd(completeness, na.rm = TRUE)
  ) %>%
  group_by(biome, HostOrder) %>%
  mutate(
    outlier = mean_cp < (quantile(mean_cp, 0.25) - 1.5 * IQR(mean_cp)) | 
      mean_cp > (quantile(mean_cp, 0.75) + 1.5 * IQR(mean_cp)),
    # Só mantém o nome se for de fato um outlier
    label_outlier = if_else(outlier, HostSpecies_SBMz, "")
  ) %>% 
  ungroup()

unique(outlier$label_outlier)

# --- GRÁFICO 2 (Com identificação e rótulo dos Outliers) ---
ggplot(all_results %>%  
         filter(completeness <= 2) %>%  
         mutate(HostOrder = fct_reorder(HostOrder, completeness, .fun = median, .desc = FALSE),
                biome = factor(BIOMAS, levels = ordem_biomas)) %>%
         mutate(biome = recode(BIOMAS,
                               "amazon_rainforest" = "Amazon",
                               "atlantic_forest" = "Atlantic Forest",
                               "cerrado" = "Cerrado",
                               "caatinga" = "Caatinga",
                               "pantanal" = "Pantanal",
                               "pampa" = "Pampa")) %>% 
         group_by(HostSpecies_SBMz, HostOrder, biome) %>%
         dplyr::summarise(
           mean_cp = mean(completeness, na.rm = TRUE),
           sd_cp = sd(completeness, na.rm = TRUE)
         ) %>%
         group_by(biome, HostOrder) %>%
         mutate(
           outlier = mean_cp < (quantile(mean_cp, 0.25) - 1.5 * IQR(mean_cp)) | 
             mean_cp > (quantile(mean_cp, 0.75) + 1.5 * IQR(mean_cp)),
           # Só mantém o nome se for de fato um outlier
           label_outlier = if_else(outlier, HostSpecies_SBMz, "")
         ) %>% 
         ungroup(),
       
       aes(x = HostOrder, y = mean_cp, fill = HostOrder)) +
  
  geom_boxplot(alpha = 0.5) +
  
  # 2. Adiciona o texto dos outliers repelidos magneticamente para não embolar
  geom_text_repel(
    aes(label = label_outlier),
    size = 2.5,                  # Tamanho da fonte do nome da espécie
    fontface = "italic",         # Nomes científicos pedem itálico!
    box.padding = 0.3,           # Espaço ao redor do texto
    point.padding = 0.2,         # Espaço para não cobrir o ponto físico do outlier
    max.overlaps = 20,           # Evita sumir com nomes caso haja muitos outliers juntos
    segment.color = "gray60",    # Cor da linhazinha que liga o texto ao ponto
    segment.size = 0.3
  ) +
  
  labs(
    x = NULL,
    y = "Completeness",
    fill = "Parasite type"
  ) +
  facet_wrap(~biome) +
  theme_projeto()
