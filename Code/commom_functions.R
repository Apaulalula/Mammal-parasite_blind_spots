# Functions

remove_after_second_underscore <- function(x) sub("^(([^_]*_){1}[^_]*).*", "\\1", x)

taxonomic_distance <- function(sp1, sp2){
  
  if(sp1$HostSpecies_SBMz == sp2$HostSpecies_SBMz){
    return(0)
  }
  
  if(sp1$HostGenus == sp2$HostGenus){
    return(1)
  }
  
  if(sp1$HostFamily == sp2$HostFamily){
    return(2)
  }
  
  if(sp1$HostOrder == sp2$HostOrder){
    return(3)
  }
  
  return(4)
}


# 1. Função de Tema Customizada
theme_projeto <- function(base_size = 11, legend= "bottom", angle= 45) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.text.x     = element_text(angle = angle, hjust = 1),
      legend.position = legend,
      strip.background = element_rect(fill = "gray95", color = NA),
      strip.text = element_text(size = 14, face = "bold", color = "black")
    )
}

# 2. Escala de Cores Customizada para o Parasite
scale_fill_parasite <- function(...) {
  scale_fill_manual(values = c("Macro" = "#D55E00", "Micro" = "#0072B2"), ...)
}

scale_color_parasite <- function(...) {
  scale_color_manual(values = c("Macro" = "#D55E00", "Micro" = "#0072B2"), ...)
}

# 3.Plot per biomes

ordem_biomas <- c("amazon_rainforest", "atlantic_forest", "cerrado", "caatinga", "pantanal","pampa")

biome_plot <- function(data){
  ggplot(data %>%
           mutate(HostOrder = fct_reorder(HostOrder, completeness, .fun = median, .desc = FALSE), 
                  biome = factor(BIOMAS, levels = ordem_biomas)) %>%
           mutate(biome = recode(BIOMAS,
                                 "amazon_rainforest" = "Amazon Rainforest",
                                 "atlantic_forest" = "Atlantic Forest",
                                 "cerrado" = "Cerrado",
                                 "caatinga" = "Caatinga",
                                 "pantanal" = "Pantanal",
                                 "pampa" = "Pampa"),
                  HostOrder = str_to_title(HostOrder)),
         aes(x = HostOrder, y = completeness, fill = Parasite)) +
    geom_boxplot(position = position_dodge(width = 0.75),
                 width = 0.65,
                 alpha = 0.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    stat_summary(
      fun = mean, 
      geom = "point", 
      position = position_dodge(width = 0.75), # Mesma largura do boxplot
      shape = 18,                              # Losango           
      size = 3, 
      color = "grey15",
    ) +
    scale_fill_parasite()+
    facet_wrap(~biome, nrow = 3,
               ncol = 2,)+
    labs(
      x = NULL,
      y = "Completeness"
    ) +
    theme_projeto()
}

# Plot completeness per number of biomes and number of studies
Completeness_nstudy_plot <- function(data, title){
  plot_data<- data %>%
  select(HostSpecies_SBMz,Parasite, BIOMAS, n_study, completeness) %>%
  group_by(HostSpecies_SBMz,Parasite) %>%
    mutate(Parasite = recode(Parasite,
                          "Micro" = "Microparasites",
                          "Macro" = "Macroparasites"
                          )) %>% 
  summarise(
    BIOMAS = n_distinct(BIOMAS),
    completeness = mean(completeness),
    n_study = mean(n_study),
    .groups = "drop"
  )
  # Linear model for R²
  mod <- lm(completeness ~ n_study, data = data)
  r2 <- summary(mod)$r.squared
  
  p <- ggplot(plot_data,
            aes(x = n_study,
                y = completeness,
                color = BIOMAS)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm",
              se = TRUE,
              color = "black",
              linewidth = 0.8) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of biomes"
  ) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = paste0("italic(R)^2 == ", round(r2, 3)),
    parse = TRUE,
    hjust = 1.1,
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = title,
    x = "Number of studies",
    y = "Completeness"
  ) +
    facet_wrap(~ Parasite)+
  theme_projeto(angle= 360)
  
   return(p)
}
