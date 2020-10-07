
### load ruminant whole-genome ML tree from Chen et al 2019 Science
### (divergence times added manually from SI, originally estimated using r8s software)

# load tree

library(ape)

rumTree <- read.nexus(here("trees/chen2019-ruminant-WGS-ML-divtimes.nex"))

plot(rumTree)
