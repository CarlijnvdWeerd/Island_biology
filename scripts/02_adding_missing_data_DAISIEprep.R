### Script computer practical practice 
renv::restore()

library(DAISIE)
library(ape)
library(DAISIEprep)

## Adding missing species to an island clade that has been sampled in the phylogeny

island_tbl@island_tbl$species
island_tbl <- add_missing_species(
  island_tbl = island_tbl,
  # num_missing_species equals total species missing
  num_missing_species = 2,
  # name of a sampled species you want to "add" the missing to
  # it can be any in the clade
  species_to_add_to = "Plant_e"
)
island_tbl@island_tbl$missing_species

data_list <- create_daisie_data(
  data = island_tbl,
  island_age = 12,
  num_mainland_species = 100,
  precise_col_time = TRUE
)

## Adding an entire lineage when a phylogeny is not available for the lineage
island_tbl <- add_island_colonist(
  island_tbl = island_tbl,
  clade_name = "Plant_y",
  status = "endemic",
  # clade with just 1 species, missing_species = 0
  # because adding the lineage already counts as 1
  missing_species = 0,
  col_time = NA_real_,
  col_max_age = FALSE,
  branching_times = NA_real_,
  min_age = NA_real_,
  clade_type = 1,
  species = "Plant_a"
)

island_tbl <- add_island_colonist(
  island_tbl = island_tbl,
  clade_name = "Plant_radiation",
  status = "endemic",
  # the total species is 5 and all are missing
  # but we add missing_species = 4 because
  # adding the lineage already counts as 1
  missing_species = 4,
  col_time = NA_real_,
  col_max_age = FALSE,
  branching_times = NA_real_,
  min_age = NA_real_,
  clade_type = 1,
  species = c("Plant_a", "Plant_b", "Plant_c",
              "Plant_d", "Plant_e")
)

# Prepare object for analyses in DAISIE
data_list <- create_daisie_data(
  data = island_tbl,
  island_age = 12,
  num_mainland_species = 100,
  precise_col_time = TRUE
)

data_list[[1]]
#> $island_age
#> [1] 12
#> 
#> $not_present
#> [1] 96

data_list[[2]]
#> $colonist_name
#> [1] "Plant_e"
#> 
#> $branching_times
#> [1] 12.0000000  0.1925924  0.1543711
#> 
#> $stac
#> [1] 2
#> 
#> $missing_species
#> [1] 2
#> 
#> $type1or2
#> [1] 1

