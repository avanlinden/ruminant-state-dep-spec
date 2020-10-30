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
  mutate(species = str_replace(species, " ", "_")) %>% 
  mutate(sd20 = as.integer(if_else(sd20 == "dimorphic", 1, 0))) %>% 
  deframe() 

#bingo
str(sd20)

# sort traits by tree tip labels

sd20 <- sd20[tree$tip.label]

# plot tree with tip traits

colvec <- rep("white", length(sd20))
colvec[sd20 == 1] <- "black"

quartz.options(height=12, width=12)
plot.phylo(tree, type = "fan", show.tip.label=F)
tiplabels(pch=21, bg=colvec, cex=0.8)

### FISSEE binary function with default arguments =================

fisseChensd20 <- FISSE.binary(tree, sd20)

pval_2tailed   <- min(fisseChensd20$pval, 1-fisseChensd20$pval)*2




\