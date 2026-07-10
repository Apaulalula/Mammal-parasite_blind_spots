pacman::p_load(tidyverse, cegan, ape)

# Load data

Host_data <- readxl::read_xlsx("./raw_data/Mammalia_SBMz_v2025-1.xlsx") %>% 
  glimpse()

# Organize df

taxa_sbmz <- Host_data %>%
  select(ORDEM, FAMILIA, GENERO, BINOMIO) %>%
  rename(HostOrder = ORDEM,
         HostFamily = FAMILIA,
         HostGenus = GENERO,
         HostSpecies_SBMz = BINOMIO) %>% 
  dplyr::mutate(across(where(is.character), str_to_lower),
                HostSpecies_SBMz = str_replace_all(HostSpecies_SBMz, " ", "_")) %>% 
  distinct()

taxa_df <- bmpo %>%
  select(HostOrder, HostFamily, HostGenus, HostSpecies_SBMz) %>%
  filter(HostSpecies_SBMz != "not_found") %>% 
  dplyr::mutate(across(where(is.character), str_to_lower)) %>% 
  distinct()

taxa_all <- bind_rows(
  taxa_sbmz,
  anti_join(taxa_df, taxa_sbmz, by = "HostSpecies_SBMz")
)

# BUild matrix
n <- nrow(taxa_all)

dist_mat <- matrix(0, n, n)

for(i in 1:n){
  
  for(j in 1:n){
    
    dist_mat[i,j] <- taxonomic_distance(
      taxa_all[i,],
      taxa_all[j,]
    )
  }
}

rownames(dist_mat) <- taxa_all$HostSpecies_SBMz
colnames(dist_mat) <- taxa_all$HostSpecies_SBMz

# Convert into distance

dist_obj <- as.dist(dist_mat)

# Make PCOA
pcoa <- cmdscale(dist_obj, k = 5, eig = TRUE)
tax_axes <- as.data.frame(pcoa$points)
pcoa$eig / sum(pcoa$eig)

colnames(tax_axes) <- paste0("TaxoPC", 1:5)

tax_axes$HostSpecies_SBMz <- rownames(tax_axes)

write.csv(tax_axes, "./data/tax.axes.csv", row.names = FALSE)
