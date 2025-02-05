### Script computer practical practice
renv::restore

# Install packages
install.packages('DAISIE',dependencies = TRUE)
install.packages('DAISIEprep',dependencies = TRUE)
install.packages('ape',dependencies = TRUE)

library(DAISIE)
library(ape)
library(DAISIEprep)

set.seed(
  4,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)
phylo <- ape::rcoal(10)

phylo$tip.label <- c("Plant_a", "Plant_b", "Plant_c", "Plant_d", "Plant_e",
                     "Plant_f", "Plant_g", "Plant_h", "Plant_i", "Plant_j")

phylo <- phylobase::phylo4(phylo)
phylobase::plot(phylo)

island_species <- data.frame(
  tip_labels = c("Plant_h",
                 "Plant_f",
                 "Plant_e")
  ,
  tip_endemicity_status = c("nonendemic","endemic","endemic"))

endemicity_status <- create_endemicity_status(
  phylo = phylo,
  island_species = island_species
)

phylod <- phylobase::phylo4d(phylo, endemicity_status)

plot_phylod(phylod = phylod)

island_tbl <- island_tbl()
island_tbl
#> Class:  Island_tbl 
#> [1] clade_name      status          missing_species col_time       
#> [5] col_max_age     branching_times min_age         species        
#> [9] clade_type     
#> <0 rows> (or 0-length row.names)

island_tbl_min <- extract_island_species(
  phylod = phylod,
  extraction_method = "min"
)
island_tbl_min
#> Class:  Island_tbl 
#>   clade_name     status missing_species  col_time col_max_age branching_times
#> 1    Plant_e    endemic               0 0.1925924       FALSE    0.154371....
#> 2    Plant_h nonendemic               0 0.1233674       FALSE              NA
#>   min_age      species clade_type
#> 1      NA Plant_e,....          1
#> 2      NA      Plant_h          1

phylod <- add_asr_node_states(phylod = phylod, asr_method = "mk")

plot_phylod(phylod = phylod)
plot_phylod(phylod = phylod, node_pies = TRUE)

island_tbl_asr <- extract_island_species(
  phylod = phylod,
  extraction_method = "asr"
)
island_tbl_asr
all.equal(island_tbl_min,island_tbl_asr)

island_tbl<-island_tbl_asr
View(island_tbl)

island_tbl@island_tbl$species
