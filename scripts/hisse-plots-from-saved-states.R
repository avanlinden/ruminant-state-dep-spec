library(hisse)
library(here)
library(tidyverse)
library(ape)
library(diversitree)

### Read in saved hisse.states marginal reconstructions =============

# list all files in output directory 
filePaths <- list.files(here("hisse-marginal-recons"), full.names = TRUE)

#select files with full paths for all chen models
chenRecPaths <- filePaths[str_detect(filePaths, "chen")]

#separate list of files without full paths fornaming
files <- list.files(here("hisse-marginal-recons"))

#create vector of names for models
chenRecNames <- files[str_detect(files, "chen")] %>% 
  str_remove(".rds")

#read in all chen tree RDS objects in directory 
chenRecObjList <- chenRecPaths %>% 
  map(readRDS)

#store as a named list
names(chenRecObjList) <- chenRecNames

#separate based on sdratio

chen_sd20RecList <-  chenRecObjList[chenRecNames[str_detect(chenRecNames, "sd20")]]
chen_sd10RecList <- chenRecObjList[chenRecNames[str_detect(chenRecNames, "sd10")]]

### Plot hisse states ===================

library(LaCroixColoR)

#model average sd20

pdf(file = here("figures/chen-hisse-states-model-avg-sd20.pdf"), width = 8, height = 8)

plot.hisse.states(
  chen_sd20RecList,
  rate.param = "net.div",
  type = "fan",
  fsize = 0.6,
  edge.width = 7,
  width.factor = 0.4,
  legend = "all",
  legend.cex = 0.8,
  rate.range = c(0.0, 0.25),
  rate.colors = rev(c(lacroix_palette("CranRaspberry", n = 6, type = "continuous"))),
  show.tip.label = FALSE,
  legend.kernel.rates = "rectangular", 
  #legend.position = c(0, 0.4, 0, 0.4),
  legend.bg = "lightgrey"
)
title(main = "Chen Tree 20% SDRatio", adj = 0, cex = 0.5)

dev.off()

#model average sd10

#pdf(file = here("figures/chen-hisse-states-model-avg-sd10.pdf"), width = 8.5, height = 11)

plot.hisse.states(
  chen_sd10RecList,
  rate.param = "net.div",
  type = "fan",
  fsize = 0.6,
  edge.width = 7,
  width.factor = 0.4,
  legend = "all",
  legend.cex = 0.8,
  rate.range = c(0.0, 0.25),
  rate.colors = rev(c(
    lacroix_palette("CranRaspberry", n = 6, type = "continuous")
  )),
  show.tip.label = FALSE,
  legend.kernel.rates = "rectangular",
  #legend.position = c(0, 0.4, 0, 0.4),
  legend.bg = "lightgrey"
)


plot.hisse.states(
  chen_sd10RecList,
  rate.param = "net.div",
  type = "fan",
  fsize = 0.6,
  edge.width = 7,
  width.factor = 0.4,
  legend = "all",
  legend.cex = 0.8,
  rate.range = c(0.0, 0.25),
  rate.colors = rev(c(
    lacroix_palette("CranRaspberry", n = 6, type = "continuous")
  )),
  show.tip.label = FALSE,
  legend.kernel.rates = "rectangular",
  #legend.position = c(0, 0.4, 0, 0.4),
  legend.bg = "lightgrey"
)
#dev.off()


### try gghisse =========

#library(gghisse)

#can use the tibbles output here to do pies at nodes for anc states

#processed_hisse <- h_process_recon(chen_sd20RecList)


###############
### Bibi models ============
###############

### Read in saved hisse.states marginal reconstructions =============

# list all files in output directory 
filePaths <- list.files(here("hisse-marginal-recons"), full.names = TRUE)

#select files with full paths for all bibi models
bibiRecPaths <- filePaths[str_detect(filePaths, "bibi")]

#separate list of files without full paths fornaming
files <- list.files(here("hisse-marginal-recons"))

#create vector of names for models
bibiRecNames <- files[str_detect(files, "bibi")] %>% 
  str_remove(".rds")

#read in all bibi tree RDS objects in directory 
bibiRecObjList <- bibiRecPaths %>% 
  map(readRDS)

#store as a named list
names(bibiRecObjList) <- bibiRecNames

#separate based on sdratio

bibi_sd20RecList <-  bibiRecObjList[bibiRecNames[str_detect(bibiRecNames, "sd20")]]
bibi_sd10RecList <- bibiRecObjList[bibiRecNames[str_detect(bibiRecNames, "sd10")]]

### Plot hisse states ===================

#model average sd20

pdf(file = here("figures/bibi-hisse-states-model-avg-sd20.pdf"), width = 8, height = 8)

plot.hisse.states(
  bibi_sd20RecList,
  rate.param = "net.div",
  type = "fan",
  fsize = 0.6,
  edge.width = 7,
  width.factor = 0.4,
  legend = "all",
  legend.cex = 0.8,
  rate.range = c(0.0, 0.25),
  rate.colors = rev(c(lacroix_palette("CranRaspberry", n = 6, type = "continuous"))),
  show.tip.label = FALSE,
  legend.kernel.rates = "rectangular", 
  #legend.position = c(0, 0.4, 0, 0.4),
  legend.bg = "lightgrey"
)
title(main = "Bibi Tree 20% SDRatio", adj = 0, cex = 0.5)

dev.off()

#model average sd10

#pdf(file = here("figures/bibi-hisse-states-model-avg-sd10.pdf"), width = 8.5, height = 11)

plot.hisse.states(
  bibi_sd10RecList,
  rate.param = "net.div",
  type = "fan",
  fsize = 0.6,
  edge.width = 7,
  width.factor = 0.4,
  legend = "all",
  legend.cex = 0.8,
  rate.range = c(0.0, 0.25),
  rate.colors = rev(c(
    lacroix_palette("CranRaspberry", n = 6, type = "continuous")
  )),
  show.tip.label = FALSE,
  legend.kernel.rates = "rectangular",
  #legend.position = c(0, 0.4, 0, 0.4),
  legend.bg = "lightgrey"
)
title(main = "Bibi 2013 mtGenome tree - 10% dimorphic", adj = 0)

#dev.off()

