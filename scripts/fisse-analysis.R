#setup

library(ape)
library(diversitree)
library(phangorn)

source(here("fisse-scripts/traitDependent_functions.R"))

tree <- read.tree(here("trees/tidy-chen-tree.tre"))

data <- read_csv(here("tidy-data/chen-body-mass-data-sdratio.csv"))

# separate out SD20 and convert to binary (1 = dimorphic, 0 = monomorphic)
# note: not playing well with tibbles

#trait values needs to be an integer vector
# deframe converts to named vector

sd20 <- data %>% 
  select(species, sd20) %>% 
  mutate(sd20 = as.integer(if_else(sd20 == "dimorphic", 1, 0))) %>% 
  deframe() 

#bingo
str(sd20)


