pacman::p_load("geobr", "sf", "ggplot2", "tidyverse", "rmapshaper")

# 1. Carregar os biomas oficiais do Brasil (ano 2019 / SIRGAS 2000)
biomas <- st_read("./shapefiles/2025_Biomas-e-Sistema-Costeiro-Marinho-do-Brasil-1-250000_shp/lml_bioma_e250k_v20250911_A.shp")

ucs <- st_read("./shapefiles/shp_cnuc_2025_03/cnuc_2025_03.shp")

ucs_simp <- ms_simplify(
  ucs,
  keep = 0.05,            # Mantém 5% dos vértices (suficiente para escala nacional)
  keep_shapes = TRUE      # Evita que pequenas UCs desapareçam
)

# 1. Alinhar CRS com o shapefile de biomas
ucs <- st_transform(ucs_simp, crs = st_crs(biomas))

# Garanta a exclusão de qualquer porção marítima recortando estritamente pelos Biomas do Brasil
# (st_intersection remove polígonos que estão além do litoral continental)
ucs_terr <- ucs_simp %>% 
  filter(is.na(marinho))

# 2. Converter o dataframe "bmpo" em um objeto espacial sf
glimpse(bmpo)

unique(taxa_bmpo$HostOrder)

taxa_bmpo <- bind_rows(
  bmpo,
  anti_join(taxa_sbmz, bmpo, by = "HostSpecies_SBMz")
)

bmpo_spatial <- bmpo %>%
  select(HostGroup, HostOrder, HostSpecies_SBMz, Domesticated_or_Wild, ParasiteCurrentName, ParGroup, ParStatus, MunicipalityCode, Biome, decimalLat, decimalLong) %>% 
  filter(Domesticated_or_Wild == "Wild") %>% 
  filter(ParGroup != "not_found",
         HostSpecies_SBMz != "not_found") %>%
  mutate(decimalLong = as.numeric(decimalLong),
         decimalLat = as.numeric(decimalLat)) %>%
  mutate(Parasite = case_when(
    ParGroup %in% c("Fungi","Protozoa", "Bacteria","RNA_virus", "DNA_virus", "Virus") ~ "Micro", 
    ParGroup %in% c("Helminth", "Arthropod") ~ "Macro")) %>% 
  glimpse()

unique(bmpo_spatial$HostOrder)

# Ajuste 'longitude', 'latitude' e 'ordem' para os nomes exatos das suas colunas
bmpo_sf <- bmpo_spatial %>%
  filter(!is.na(decimalLong) & !is.na(decimalLat)) %>%
  st_as_sf(coords = c("decimalLong", "decimalLat"), crs = 4326) %>%
  # Reprojeta para o mesmo CRS do geobr (SIRGAS 2000 / EPSG 4674)
  st_transform(crs = st_crs(biomas))

# 3. Construir o mapa
mapa <- ggplot() +
  # Camada dos biomas
  geom_sf(
    data = biomas,
    aes(fill = NM_BIOMA),
    color = "white",
    linewidth = 0.2,
    alpha = 0.45
  ) +
  # Camada 2: Unidades de Conservação (UCs)
  geom_sf(
    data = ucs_terr,
    fill = "grey85",        # Verde escuro florestal
    color = "grey80",       # Borda ligeiramente mais escura
    linewidth = 0.1,
    alpha = 0.65,            # Transparência para não sobrecarregar
    show.legend = FALSE      # Oculta legenda das UCs se não for categorizá-las
  ) +
  # Camada dos pontos de ocorrência por ordem
  geom_sf(
    data = bmpo_sf,
    aes(color = "grey50",
        alpha = 0.8)
  ) +
  # Paletas de cores customizadas
  guides(color = "none", 
         size = "none",
         alpha = "none") +
  scale_fill_brewer(palette = "Set3", name = "Bioma") +
  scale_color_viridis_d(option = "turbo") +
  # Ajuste de coordenadas para manter os limites do Brasil continental
  coord_sf(xlim = c(-74, -34), ylim = c(-34, 6)) +
  facet_wrap(~HostOrder)+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.15),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 13)
  ) +
  labs(
    title = "Mammal-parasite associations registered in Brazil",
    x = "Longitude",
    y = "Latitude"
  )

print(mapa)

ggsave(
  filename = "./Figures/mapa_mamiferos_biomas.png",
  plot = mapa,
  width = 15,
  height = 10,
  units = "in",
  bg = "white"
)

# 3. Construir o mapa para micro
mapa_micro <- ggplot() +
  # Camada dos biomas
  geom_sf(
    data = biomas,
    aes(fill = NM_BIOMA),
    color = "white",
    linewidth = 0.2,
    alpha = 0.45
  ) +
  # Camada 2: Unidades de Conservação (UCs)
  geom_sf(
    data = ucs_terr,
    fill = "grey85",        # Verde escuro florestal
    color = "grey80",       # Borda ligeiramente mais escura
    linewidth = 0.1,
    alpha = 0.65,            # Transparência para não sobrecarregar
    show.legend = FALSE      # Oculta legenda das UCs se não for categorizá-las
  ) +
  # Camada dos pontos de ocorrência por ordem
  geom_sf(
    data = bmpo_sf %>% 
      filter(Parasite == "Micro"),
    aes(color = "grey50",
        alpha = 0.8)
  ) +
  # Paletas de cores customizadas
  guides(color = "none", 
         size = "none",
         alpha = "none") +
  scale_fill_brewer(palette = "Set3", name = "Bioma") +
  scale_color_viridis_d(option = "mako") +
  # Ajuste de coordenadas para manter os limites do Brasil continental
  coord_sf(xlim = c(-74, -34), ylim = c(-34, 6)) +
  facet_wrap(~HostOrder)+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.15),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 13)
  ) +
  labs(
    title = "Mammal-microparasite associations registered in Brazil",
    x = "Longitude",
    y = "Latitude"
  )

print(mapa_micro)

ggsave(
  filename = "./Figures/mapa_mamiferos_biomas.png",
  plot = mapa_micro,
  width = 15,
  height = 10,
  units = "in",
  bg = "white"
)