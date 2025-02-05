### Script computer practical practice 
renv::restore()

library(DAISIE)
library(ape)
library(DAISIEprep)
# Fitting DAISIE models using the Galápagos birds as example

rm(list = ls())
galapagos_datalist
View(galapagos_datalist)

# To view data in a different way
DAISIE_plot_island(galapagos_datalist)

# plot age vs diversity
DAISIE_plot_age_diversity(galapagos_datalist)

# Fit the DAISIE model

# Fit a full DAISIE model (M1)
# The first model (M1) we will fit is a full DAISIE model with 5 parameters. The parameters in DAISIE are always placed in this order:
# Cladogenesis rate (lambda_c) (unit: cladogenesis events per island lineage # per time unit)
# Extinction rate (mu) (unit: extinction events per island lineage per time # #unit)
# Carrying capacity (K) (unit: number of species)
# Colonisation rate (gamma) (unit: colonisation events per mainland lineage per # time unit)
# Anagenesis rate (lambda_a) (unit: anagenesis events per island lineage per # time unit)

M1_results <- DAISIE_ML(
  datalist = galapagos_datalist,
  initparsopt = c(1.5,1.1,20,0.009,1.1),
  ddmodel = 11,
  idparsopt = 1:5,
  parsfix = NULL,
  idparsfix = NULL
)

M1_results
#>   lambda_c       mu       K       gamma     lambda_a    loglik df conv
#> 1 1.258226 1.136924 9968506 0.004957378 1.251793e-06 -84.78145  5    0

# Fit model with no carrying-capacity (M2)
M2_results <- DAISIE_ML(
  datalist = galapagos_datalist,
  initparsopt = c(1.5,1.1,0.009,1.1),
  idparsopt = c(1,2,4,5),
  parsfix = Inf,
  idparsfix = 3,
  ddmodel = 0
)

M2_results
#>   lambda_c       mu   K      gamma     lambda_a    loglik df conv
#> 1 1.264389 1.149378 Inf 0.00505558 1.662578e-05 -84.78088  4    0

# Fit model with no carrying capacity AND no anagenesis (M3)
M3_results <- DAISIE_ML(
  datalist = galapagos_datalist,
  initparsopt = c(1.5,1.1,0.009),
  idparsopt = c(1,2,4),
  parsfix = c(Inf,0),
  idparsfix = c(3,5),
  ddmodel = 0
)

M3_results
#>   lambda_c       mu   K       gamma lambda_a    loglik df conv
#> 1 1.263034 1.146225 Inf 0.005040353        0 -84.78082  3    0

# Select the best model using AIC
AIC_compare <- function(LogLik,k){
  aic <- (2 * k) - (2 * LogLik)
  return(aic)
}

AICs <- AIC_compare(LogLik = c(M1_results$loglik,M2_results$loglik,M3_results$loglik),
                    k = c(M1_results$df,M2_results$df,M3_results$df))
names(AICs) <- c('M1','M2','M3')
AICs
#>       M1       M2       M3 
#> 179.5629 177.5618 175.5616
# The model with the lowest AIC value is the preferred model (In this case, M3)

# Simulate islands
# Simulate islands with the parameters estimated from the best model for the Galápagos bird data (takes a while to run, you can reduce number of replicates if you want)
Galapagos_sims <- DAISIE_sim(
  time = 4,
  M = 1000,
  pars = c(1.5, 2.0, Inf, 0.010,0),
  replicates = 100,
  plot_sims = FALSE)

# Plot the species-through-time plots resulting from the simulations
DAISIE_plot_sims(Galapagos_sims)
# If extinction rate is higher than cladogenesis rate, then the number of species will reach a plateau and eventually decrease. This is what we see in the Galápagos bird data.

# Comparison with bird data from the Azores islands
data(Macaronesia_datalist)
Azores <- Macaronesia_datalist[[1]]
DAISIE_plot_island(Azores)

Azores_sims <- DAISIE_sim(
  time = 6.3,
  M = 300,
  pars = c(0,1.053151832,Inf,0.052148979,0.512939011),
  replicates = 100,
  plot_sims = FALSE)

DAISIE_plot_sims(Azores_sims)
# The Azores bird data reached a plateau in the number of species, while the Galapagos bird data (in the normal settings) did not reach that plateau. 

