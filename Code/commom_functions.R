# Functions

remove_after_second_underscore <- function(x) sub("^(([^_]*_){1}[^_]*).*", "\\1", x)

# Taxonomic distance function ----

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

# Build mammal phylogenetic tree ----

enxertar_especie_robusta <- function(tr, sp_nova, lista_irmas = NULL, genero = NULL) {
  if (is.null(tr$edge.length) || any(is.na(tr$edge.length))) {
    tr <- compute.brlen(tr, 1)
  }
  
  # A. Tenta enxertar como irmã da espécie conhecida
  if (!is.null(lista_irmas)) {
    irma_presente <- lista_irmas[lista_irmas %in% tr$tip.label][1]
    
    if (!is.na(irma_presente)) {
      tip_idx <- which(tr$tip.label == irma_presente)
      edge_idx <- which(tr$edge[, 2] == tip_idx)
      branch_len <- tr$edge.length[edge_idx]
      if (is.na(branch_len) || branch_len <= 0) branch_len <- 1e-4
      
      message(paste("-> Enxertando", sp_nova, "como irma de", irma_presente))
      return(bind.tip(
        tree = tr,
        tip.label = sp_nova,
        where = tip_idx,
        position = branch_len / 2,
        edge.length = branch_len / 2
      ))
    }
  }
  
  # B. Fallback para congêneres / MRCA do gênero
  congeneres <- grep(paste0("^", genero, "_"), tr$tip.label, value = TRUE)
  
  if (length(congeneres) == 1) {
    tip_idx <- which(tr$tip.label == congeneres)
    edge_idx <- which(tr$edge[, 2] == tip_idx)
    branch_len <- tr$edge.length[edge_idx]
    if (is.na(branch_len) || branch_len <= 0) branch_len <- 1e-4
    
    message(paste("-> Enxertando", sp_nova, "no unico congenere:", congeneres))
    return(bind.tip(
      tree = tr,
      tip.label = sp_nova,
      where = tip_idx,
      position = branch_len / 2,
      edge.length = branch_len / 2
    ))
    
  } else if (length(congeneres) > 1) {
    mrca_node <- findMRCA(tr, congeneres)
    dist_mrca_to_tip <- nodeheight(tr, which(tr$tip.label == congeneres[1])) - nodeheight(tr, mrca_node)
    if (dist_mrca_to_tip <= 0) dist_mrca_to_tip <- 1e-4
    
    message(paste("-> Enxertando", sp_nova, "na base (MRCA) do genero:", genero))
    return(bind.tip(
      tree = tr,
      tip.label = sp_nova,
      where = mrca_node,
      position = 0,
      edge.length = dist_mrca_to_tip
    ))
    
  } else {
    warning(paste("Pulando", sp_nova, "- Nenhum representante para o genero:", genero))
    return(tr)
  }
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
    facet_wrap(~biome, ncol= 2, nrow=3)+
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
