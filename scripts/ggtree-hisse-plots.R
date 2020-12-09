
### Plotting hisse results with ggtree

library(ggtree)
library(ggimage)
library(gghisse)
library(LaCroixColoR)
library(ggpubr)
library(ggnewscale)

### get treedata object with model averaged rate and state data ===========

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

# use gghisse to process/model average

bibiProc20 <- h_process_recon(bibi_sd20RecList)[[1]] #needed this for ggtree/ape

bibiProc20 <- h_process_recon(bibi_sd20RecList) #need this for gghisse

### plot tree with pies at nodes for states =========

#get nodes likelihoods as df

bibiNodes20 <- bibiProc20@data %>% 
  slice(114:n()) %>% 
  select(node = id, dimorphic = state) %>% 
  mutate(monomorphic = 1 - dimorphic) %>% 
  as.data.frame()

#get node pie colors

pieCols <- c("white", "black")
names(pieCols) <- c("monomorphic", "dimorphic")

#create node pies using my function which doesn't suck
source(here("scripts/better-nodepie-function.R"))
pies <- nodepie2(bibiNodes20, cols = 2:3, line_color = "black")
pies <- lapply(pies, function(g) g + scale_fill_manual(values = pieCols))

#designate tip colors
tipCols = c("0" = "white", "1" = "black")

#plot tree with branches colored by div rate, pies as insets, and states as tippoints

ggtree(bibiProc20@phylo, aes(color = bibiProc20@data$net.div), size = 1.5) +
  scale_color_gradientn(colors = rev(c(
    lacroix_palette("CranRaspberry", n = 6, type = "continuous")
  ))) +
  geom_inset(pies,
             width = 0.03,
             height = 0.03,
             hjust = 0.1) +
  labs(color = "div rate") +
  ggnewscale::new_scale_color() +
  geom_tippoint(size = 1.8, position = position_nudge(x = 0.5, y = 0)) +
  scale_fill_manual(values = "black") +
  ggnewscale::new_scale_color() +
  geom_tippoint(aes(color = as.factor(bibiProc20@data$state)),
                size = 1,
                position = position_nudge(x = 0.5, y = 0)) +
  scale_color_manual(values = tipCols) +
  labs(color = "state", title = "Bibi Tree with 20% SexDimorph Ratio")

ggsave(here("figures/bibi-sd20-ggtree-rates-pies.png"))

### now do it with the chen tree =============

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

# gghisse processing, subset for ggtree

chenProc20 <- h_process_recon(chen_sd20RecList)[[1]]

#node data
chenNodes20 <- chenProc20@data %>% 
  slice(52:n()) %>% 
  select(node = id, dimorphic = state) %>% 
  mutate(monomorphic = 1 - dimorphic) %>% 
  as.data.frame()
#get node pie colors

pieCols <- c("white", "black")
names(pieCols) <- c("monomorphic", "dimorphic")

#create node pies using my function which doesn't suck
source(here("scripts/better-nodepie-function.R"))
chen_pies <- nodepie2(chenNodes20, cols = 2:3, line_color = "black")
chen_pies <- lapply(pies, function(g) g + scale_fill_manual(values = pieCols))

  