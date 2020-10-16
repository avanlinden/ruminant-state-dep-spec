
### load ruminant whole-genome ML tree from Chen et al 2019 Science
### (divergence times added manually from SI, originally estimated using r8s software)

# load tree

library(ape)

chenTree <- read.nexus(here("trees/chen2019-ruminant-WGS-ML-divtimes.nex"))

#check
plot(chenTree)

#extract tip names

write_csv(as.data.frame(chenTree$tip.label), here("raw-data/raw-tip-labels.csv"), quote_escape = "double")

# reimport scientific tip names

chenNames <- read_csv(here("raw-data/sci-names-tip-labels.csv")) %>% 
  rename("common" = 1, "species" = 2)

chenNames
