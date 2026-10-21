# Surveillance Blind Spots in Mammal–Parasite Interactions Across Brazilian Biomes

[![R Version](https://img.shields.io/badge/R-%E2%89%A54.6.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository hosts the data, data processing pipelines, machine learning models (Extreme Gradient Boosting – XGBoost), and analytical scripts for the manuscript:

> **Costa, A. P. L., Cruz, G. L. T., Winck, G. R., & Andreazzi, C. S.** (2026). *Surveillance Blind Spots in Mammal–Parasite Interactions Across Brazilian Biomes*.

---

## 📌 Overview

Estimating parasite diversity is a persistent challenge in disease ecology and biodiversity conservation due to the cryptic nature of parasites and severe sampling biases. This study evaluates surveillance and taxonomic blind spots in mammal–parasite associations across all Brazilian biomes (Amazon Rainforest, Atlantic Forest, Caatinga, Cerrado, Pampa, and Pantanal).

By combining host phylogenetic distances, ecological traits (body mass, diet diversity), environmental variables (biomes, habitat classifications, IUCN threat status, synanthropy), and sampling effort (number of studies), we trained XGBoost regression models to predict the unobserved richness of:
- **Macroparasites:** Helminths and parasitic arthropods.
- **Microparasites:** Viruses, bacteria, protozoa, and fungi.

---

## 📂 Repository Structure

```text
.
├── Code/                      # R scripts for data processing, modeling, and plotting
│   ├── 01_Municipios.R        # Including Biomes information from the integration with Brazilian municipalities
│   ├── 02_BuildTree.R         # Subseting global mammal phylogeny
│   ├── 02_TaxonomicDistance.R # PCoA of cophenetic/taxonomic distance matrices
│   ├── 03_IUCN_habitat.R      # Extraction of IUCN habitat classifications & threat status
│   ├── 04_MammalTraits.R      # Integration of host features
│   ├── 05_Model_train.R       # 5-fold cross-validation and XGBoost model training
│   ├── 06_Model_prediction.R  # Potential richness prediction & completeness
│   ├── 07_Maps_gaps.R         # Generation of spatial gap maps
│   ├── commom_functions.R     # Utility functions shared across analytical scripts
│   ├── plot_outliers.R        # Diagnostics and visualization of model residuals/outliers
│   └── Habitat_IUCN/          # Gathering IUCN habitat information 
│
├── Data/                      # Processed datasets and intermediate tables
│   ├── Biome_all.csv
│   ├── Biome_final.rds
│   ├── Biome_Not_found.csv
│   ├── Bmpo_Biome_all.csv
│   ├── df_model.csv           # Final dataset used for XGBoost training and testing
│   ├── Host_traits.csv        # Mammal ecological traits
│   ├── host_sinantropy.csv    # Anthropogenic habitat association scores
│   ├── mcp_biome.csv
│   ├── tax.axes.csv           # Extracted PCoA phylogenetic ordination axes
│   └── tree_final_mammals.tree # Pruned phylogenetic tree of Brazilian mammals
│
├── Figures/                   # Generated output figures and manuscript visualisations
│   ├── dens_biome.png
│   ├── High_NoStudyEffort.png
│   ├── no_doc_biome.png
│   ├── no_doc_n.png
│   ├── Observed_vs_Predicted_HighStudyEffort.png
│   ├── Observed_vs_Predicted_NoStudyEffort.png
│   ├── Order_per_biome1.png
│   ├── plot_n_biome.png
│   ├── radar_biome.png
│   ├── Species_number_biome.png
│   └── Species_per_biome.png
│
├── Output/                    # Serialized trained model objects
│   ├── model_ns.model         # Fitted XGBoost model without sampling effort
│   └── model_s.model          # Fitted XGBoost model with sampling effort
│
├── Raw_data/                  # Primary input data (BMPO, IUCN schemes, BSMz lists)
└── README.md
