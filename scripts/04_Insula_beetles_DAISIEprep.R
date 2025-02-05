### Script computer practical Insula beetles
renv::restore()

library(DAISIE)
library(ape)
library(DAISIEprep)
install.packages('readxl',dependencies = TRUE)
library(readxl)
install.packages('tidyverse',dependencies = TRUE)
library(tidyverse)
install.packages('dplyr',dependencies = TRUE)
library(dplyr)

rm(list = ls())

# Load dataset of insula tree into R 
phylo <- read.nexus("C:/Users/carli/_projects/Island Bio/Insula.tre")

phylo <- phylobase::phylo4(phylo)
phylobase::plot(phylo)

# Load checklist of insula into R 
checklist <- read_excel("C:/Users/carli/_projects/Island Bio/Insula_checklist.xlsx")

# Create name code to match the phylogeny and checklist. In other words, I want to add a column in R dataset wherein i combine to other columns. So, taxon and code need to be combined in a new colum.
checklist <- checklist |>
  dplyr::mutate(name_code = paste0(Taxon, "_", Code)) |>
  dplyr::filter(Island=='Jamaica')

# Create a dataframe for island species 

island_species <- data.frame(
  tip_labels = c("Spec_9",
                 "Spec_19",
                 "Spec_24",
                 "Spec_25",
                 "Spec_26",
                 "Spec_29",
                 "Spec_33",
                 "Spec_38",
                 "Spec_39",
                 "Spec_40",
                 "Spec_41",
                 "Spec_42",
                 "Spec_43",
                 "Spec_47",
                 "Spec_48",
                 "Spec_51",
                 "Spec_52")
  ,
  tip_endemicity_status = c("endemic","endemic","nonendemic", "endemic", "endemic", "endemic", "endemic", "endemic", "endemic", "endemic", "endemic", "endemic",  "endemic", "endemic", "endemic", "endemic", "endemic"))

endemicity_status <- create_endemicity_status(
  phylo = phylo,
  island_species = island_species)
### Missing species in the phylogeny are 51 & 52

## add the endemicity status to the phylogeny
phylod <- phylobase::phylo4d(phylo, endemicity_status)
plot_phylod(phylod = phylod)

# Create an island table which is for now empty
island_tbl <- island_tbl()
island_tbl

## Extract the island species from the phylogeny with either min
island_tbl_min <- extract_island_species(
  phylod = phylod,
  extraction_method = "min"
)
island_tbl_min

phylod <- add_asr_node_states(phylod = phylod, asr_method = "mk")

plot_phylod(phylod = phylod)
plot_phylod(phylod = phylod, node_pies = TRUE)

# or with asr
island_tbl_asr <- extract_island_species(
  phylod = phylod,
  extraction_method = "asr"
)
island_tbl_asr
all.equal(island_tbl_min,island_tbl_asr)
# both are equal so will use asr

island_tbl<-island_tbl_asr
View(island_tbl)

island_tbl@island_tbl$species

## Adding species 51 to the phylogeny tree
island_tbl <- add_island_colonist(
  island_tbl = island_tbl,
  clade_name = "Spec_51",
  status = "endemic",
  missing_species = 0,
  col_time = NA_real_,
  col_max_age = FALSE,
  branching_times = NA_real_,
  min_age = NA_real_,
  clade_type = 1,
  species = "Spec_51"
)

## Adding species 52 to the phylogeny tree
island_tbl@island_tbl$species
island_tbl <- add_missing_species(
  island_tbl = island_tbl,
  num_missing_species = 1,
  species_to_add_to = "Spec_42")

island_tbl@island_tbl$species
island_tbl@island_tbl$missing_species
## confirmed that 52 is added to the clade as missing species


# Prepare object for analyses in DAISIE
data_list <- create_daisie_data(
  data = island_tbl,
  island_age = 5,
  num_mainland_species = 1000,
  precise_col_time = TRUE
)
