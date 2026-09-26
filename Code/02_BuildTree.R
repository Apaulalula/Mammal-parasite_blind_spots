# LOAD LIBRARIES
pacman::p_load(ape, tidyverse, tidytree, treeio, caret,phytools, ggtree, ggplot2, pheatmap,stringr, grid)

# Define local and data directories
localDir = "."
data.directory = file.path(localDir, "raw_data")

# Read .tree file
mammal_tree <- list.files(data.directory, pattern = "\\.nex$", full.names = TRUE)

# Load tree
mammal_tree <- lapply(mammal_tree, read.nexus)

# Select the first tree as representative
phylo_tree_mammal_full <- mammal_tree[[1]]

# 1. Normalizar o dataframe e eliminar valores inválidos como "not_found"
mammal_species <- unique(str_replace_all(bmpo$HostSpecies_SBMz, "\ufb01", "fi"))
mammal_species <- mammal_species[!is.na(mammal_species) & mammal_species != "not_found"]
mammal_species <- str_squish(mammal_species)
mammal_species <- str_replace_all(mammal_species, "ﬁ", "fi")

# Harmonise species names
phylo_tree_mammal_full$tip.label <- remove_after_second_underscore(phylo_tree_mammal_full$tip.label)

phylo_tree_mammal_full$tip.label <- tolower(phylo_tree_mammal_full$tip.label)

# Subset tree to only include species present in the matrices
no_phylo <- setdiff(mammal_species, phylo_tree_mammal_full$tip.label)

tips_arvore <- phylo_tree_mammal_full$tip.label
faltantes   <- sort(unique(no_phylo))

# 2. Diagnóstico: busca por aproximação de texto e congêneres
diagnostico_sinonimos <- lapply(faltantes, function(sp) {
  gen <- str_extract(sp, "^[a-z]+")
  epiteto <- str_remove(sp, "^[a-z]+_")
  
  # A. Congêneres presentes na árvore
  congeneres_na_arvore <- grep(paste0("^", gen, "_"), tips_arvore, value = TRUE)
  
  # B. Pontas na árvore com o mesmo epíteto específico (gênero mudou)
  mesmo_epiteto <- grep(paste0("_", epiteto, "$"), tips_arvore, value = TRUE)
  
  # C. Pontas com grafia mais próxima (Levenshtein distance <= 3)
  distancias <- adist(sp, tips_arvore)[1, ]
  nomes_proximos <- tips_arvore[distancias <= 3]
  
  data.frame(
    especie_sbmz = sp,
    genero = gen,
    tem_congenere_na_arvore = length(congeneres_na_arvore) > 0,
    n_congeneres = length(congeneres_na_arvore),
    candidatos_mesmo_epiteto = paste(mesmo_epiteto, collapse = "; "),
    candidatos_grafia_parecida = paste(nomes_proximos, collapse = "; ")
  )
}) %>% bind_rows()

# Visualizar o resultado
print(diagnostico_sinonimos)

# 2. Reiniciar a árvore original completa
tree <- phylo_tree_mammal_full

# 3. Dicionário de sinonímias e variações ortográficas diretas na árvore
# (Nome na Árvore Original -> Nome Padronizado SBMz)
sinonimos_diretos <- c(
  "pecari_tajacu"           = "dicotyles_tajacu",
  "mimon_crenulatum"        = "gardnerycteris_crenulatum",
  "sciurus_aestuans"        = "guerlinguetus_aestuans",
  "sciurus_spadiceus"       = "hadrosciurus_spadiceus",
  "pseudalopex_gymnocercus" = "lycalopex_gymnocercus",
  "pseudalopex_vetulus"     = "lycalopex_vetulus",
  "vampyressa_bidens"       = "vampyriscus_bidens",
  "leontocebus_fuscicollis"  = "saguinus_fuscicollis",
  "leontocebus_weddelli"     = "saguinus_weddelli",
  "diaemus_youngi"           = "diaemus_youngii",
  "lagothrix_lagotricha"     = "lagothrix_lagothrica",
  "lasiurus_blossevillii"    = "lasiurus_blossevilli",
  "lophostoma_silvicolum"    = "lophostoma_silvicola",
  "marmosa_paraguayanus"    = "marmosa_paraguayana",
  "rhinophylla_fischerae"    = "rhinophylla_fischerae"
)

# Aplicar as renomeações diretas nas pontas
for (arvore_nome in names(sinonimos_diretos)) {
  novo_nome <- sinonimos_diretos[[arvore_nome]]
  if (arvore_nome %in% tree$tip.label) {
    tree$tip.label[tree$tip.label == arvore_nome] <- novo_nome
  }
}

# 4. Checar espécies que restaram para enxerto
restantes_ausentes <- setdiff(mammal_species, tree$tip.label)
cat("Total de espécies para enxerto (devem ser 15):", length(restantes_ausentes), "\n")
print(restantes_ausentes)

# 5. Mapeamento de espécies-irmãs prioritárias
irmas_prioritarias <- list(
  "aotus_infulatus"           = c("aotus_azarae", "aotus_nigriceps"),
  "dasyprocta_variegata"      = c("dasyprocta_punctata", "dasyprocta_croconota"),
  "eptesicus_taddeii"         = c("eptesicus_brasiliensis", "eptesicus_diminutus"),
  "histiotus_diaphanopterus"  = c("histiotus_velatus", "histiotus_macrotus"),
  "hylaeamys_seuanezi"        = c("hylaeamys_megacephalus", "hylaeamys_laticeps"),
  "leopardus_guttulus"        = c("leopardus_tigrinus"),
  "lichonycteris_degener"     = c("lichonycteris_obscura"),
  "marmosa_rutteri"           = c("marmosa_regina"),
  "metachirus_myosuros"       = c("metachirus_nudicaudatus"),
  "natalus_macrourus"         = c("natalus_stramineus"),
  "oligoryzomys_mattogrossae" = c("oligoryzomys_fornesi", "oligoryzomys_flavescens"),
  "oligoryzomys_utiaritensis" = c("oligoryzomys_eliurus", "oligoryzomys_nigripes"),
  "philander_quica"           = c("philander_frenatus", "philander_opossum"),
  "platyrrhinus_incarum"      = c("platyrrhinus_helleri"),
  "pteronotus_rubiginosus"    = c("pteronotus_parnellii")
)

# 7. Execução do enxerto
tree_imputada <- tree

for (sp in restantes_ausentes) {
  gen <- str_extract(sp, "^[a-z]+")
  candidatas_irmas <- irmas_prioritarias[[sp]]
  
  tree_imputada <- enxertar_especie_robusta(
    tr = tree_imputada,
    sp_nova = sp,
    lista_irmas = candidatas_irmas,
    genero = gen
  )
}

# 8. Regularização topológica e poda final
tree_imputada <- force.ultrametric(tree_imputada, method = "extend")
tree_imputada <- collapse.singles(tree_imputada)

tree_final <- keep.tip(tree_imputada, intersect(especies_necessarias, tree_imputada$tip.label))

# 9. Diagnóstico final
cat("\n=== STATUS FINAL ===\n")
cat("Total de espécies necessárias:     ", length(especies_necessarias), "\n")
cat("Total de pontas na árvore podada:   ", Ntip(tree_final), "\n")
cat("Espécies ainda faltantes:           ", length(setdiff(especies_necessarias, tree_final$tip.label)), "\n")
cat("Árvore ultramétrica:               ", is.ultrametric(tree_final), "\n")

