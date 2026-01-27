Integrating databases for spatial analysis of parasite-host associations 
and the novel Brazilian dataset 
Gabriella L. T. Cruz, Gisele R. Winck, Paulo S. D’Andréa, 
Eduardo Krempser, Mariana M. Vidal, Cecilia S. Andreazzi

Funding: MCTI/CNPq Sinbiose.

## This README.text file includes the basic information about the scripts files.

For data processing and generation of data products, we relied on open-source 
software packages. Specific software versions, when relevant, have been duly 
specified in the manuscript. The R code used to retrieve data from NCBI Nucleotide, GBIF, EID2, 
and NHM datasets is available at figshare. Codes for cross-referencing 
Nucleotide and PubMed, as well as meta-network analysis, are housed in the 
same repository.

# R Code: Dowloading Host-Parasite Data From The EID2
R code used to search host–parasite association records 
from the Enhanced Infectious Diseases database (EID2).
file name: script01_parasites_eid2

# R Code: Dowloading Host-Parasite Data From The London Museum
R code used to search host–helminth parasite occurrence records 
from the London Natural History Museum database (NHM).
file name: script02_helminth_R_LondonMuseum

# R Code: Dowloading Zoonotic Microparasite Data From NCBI Nucleotide
R code used to search zoonotic microparasite data from the 
NCBI Nucleotide database.
file name: script03_zoonotic_microparasite_nucleotide

# R Code: Dowloading Zoonotic Microparasite Data From GBIF
R code used to search zoonotic microparasite data at the 
Global Biodiversity Information Facility database (GBIF).
file name: script04_zoonotic_microparasite_gbif

# R Code: Cross-Reference At The NCBI Nucleotide and PubMed
R code used to search zoonotic microparasite cross-reference at the 
NCBI Nucleotide database and the PubMed platform.
file name: script05_zoonotic_microparasite_nucleotide_pubmed

# R Code: Mammal Reference At The PubMed
R code used to search reference for Brazilian wild mammals 
at the PubMed platform.
file name: script06_mammal_sbmz_pubmed

# R Code: Calculating Network Centralities
R code used to get the centrality metrics to conduct the meta-network 
analyses.
file name: script07_meta-network_centralities
