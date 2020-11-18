library(ape)
library(diversitree)
library(phangorn)

source(here("fisse-scripts/traitDependent_functions.R"))

### FISSE wrapper function using FISSE.binary default parameters (parsimony cutoff = 0.1, Mk1)
# inputs: phylogenetic tree with same # of tips as data, data with species and sdratio, sdratio cutoff value (1 = dimorphic, 0 = monomorphic)

dimorphFISSE <- function(tree, data, threshold) {
  
  #create binary integer dataframe from sdratio data at specified threshold
  binData <- data %>% 
    mutate(binary = as.integer(if_else(sdratio >= threshold, 1, 0))) %>% 
    select(species, binary) %>% 
    deframe()
  
  #sort binary state data by tree tip labels
  binData <- binData[tree$tip.label]
  
  #run FISSE functions with default parameters
  FISSE.binary(tree, binData)
  
}

