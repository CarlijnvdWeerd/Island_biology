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

#### DAISIE 
View(data_list)

# To view data in a different way
DAISIE_plot_island(data_list)

# plot age vs diversity
DAISIE_plot_age_diversity(data_list)
### RADIATION 

# Fit a full DAISIE model (M1)
M1_results <- DAISIE_ML(
  datalist = data_list,
  initparsopt = c(5.8,7.7,20,0.02,3.2),
  ddmodel = 11,
  idparsopt = 1:5,
  parsfix = NULL,
  idparsfix = NULL
)

M1_results
#  lambda_c       mu       K      gamma lambda_a    loglik df conv
#  5.90395 7.821783 2947851 0.02523945  3.19606 -37.10743  5    0

# Fit model with no carrying-capacity (M2)
M2_results <- DAISIE_ML(
  datalist = data_list,
  initparsopt = c(5.8,7.7,0.02,3.2),
  idparsopt = c(1,2,4,5),
  parsfix = Inf,
  idparsfix = 3,
  ddmodel = 0
)

M2_results
# lambda_c       mu   K      gamma lambda_a    loglik df conv
# 5.904042 7.822047 Inf 0.02522024  3.19712 -37.10743  4    0

# Fit model with no carrying capacity AND no anagenesis (M3)
M3_results <- DAISIE_ML(
  datalist = data_list,
  initparsopt = c(5.8,7.7,0.02),
  idparsopt = c(1,2,4),
  parsfix = c(Inf,0),
  idparsfix = c(3,5),
  ddmodel = 0
)

M3_results
#  lambda_c       mu   K      gamma lambda_a    loglik df conv
#   6.76418 8.775854  Inf 0.02762404      0  -37.30016  3    0

# Select the best model using AIC
AIC_compare <- function(LogLik,k){
  aic <- (2 * k) - (2 * LogLik)
  return(aic)
}

AICs <- AIC_compare(LogLik = c(M1_results$loglik,M2_results$loglik,M3_results$loglik),
                    k = c(M1_results$df,M2_results$df,M3_results$df))
names(AICs) <- c('M1','M2','M3')
AICs
## Best model is M3: 80.60033

# Simulate islands with the parameters estimated from the best model for the bird data
Jamaica_sims <- DAISIE_sim(
  time = 5,
  M = 1000,
  pars = c(6.76418,8.775854,Inf,0.02762404,0),
  replicates = 100,
  plot_sims = FALSE)

# Plot the species-through-time plots resulting from the simulations
DAISIE_plot_sims(Jamaica_sims)

### Run extra simulations 
# One where colonization (8.77) and extinction rates (3.0)
Jamaica_sims_extra1 <- DAISIE_sim(
  time = 5,
  M = 1000,
  pars = c(6.76418,3.0 ,Inf,8.775854,0),
  replicates = 100,
  plot_sims = FALSE)

DAISIE_plot_sims(Jamaica_sims_extra1)

# One where the cladogenesis is higher than the extinction rate
Jamaica_sims_extra2 <- DAISIE_sim(
  time = 5,
  M = 1000,
  pars = c(8.775854,6.76418,Inf,0.02762404,0.1),
  replicates = 100,
  plot_sims = FALSE)

DAISIE_plot_sims(Jamaica_sims_extra2) 