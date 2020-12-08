
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

#get colors

#pieCols <- lacroix_palette("PeachPear", n = 2, type = "discrete")
pieCols <- c("white", "black")
names(pieCols) <- c("monomorphic", "dimorphic")

#create pies
source(here("scripts/better-nodepie-function.R"))
pies <- nodepie2(bibiNodes20, cols = 2:3, line_color = "black")
pies <- lapply(pies, function(g) g + scale_fill_manual(values = pieCols))

ggtree(bibiProc20@phylo, size = 0.25) +
  geom_inset(pies, width = 0.04, height = 0.04, hjust = 0.1) +
  theme_tree2()

# gghisse rate tree

lacroix_palettes$CranRaspberry[6]

rateCols <- c(lacroix_palettes$CranRaspberry[1,6], lacroix_palettes$CranRaspberry[1,1])

ratePlot <- h_rate_recon(bibiProc20,
                         colors = rateCols)
ratePlot +
  geom_tree(size = 1.5) +
  geom_inset(pies, width = 0.07, height = 0.07, hjust = 0.1) +
  geom_tippoint(aes(color = state))
  #scale_color_discrete(c("white", "black"))

#get tip states

bibiTipStates <- bibiProc20@data %>% 
  slice(1:113) %>% 
  select(id, state) %>% 
  column_to_rownames(var = "id") %>% 
  as.data.frame() 

# fuck gghisse, fuck the ggtree documentation, just try from scratch

#use model averaged treedata object
class(bibiProc20)

tipCols = c("0" = "white", "1" = "black")

ggtree(bibiProc20@phylo, aes(color = bibiProc20@data$net.div), size = 1.5) +
  scale_color_gradientn(colors = rev(c(lacroix_palette("CranRaspberry", n =6, type = "continuous")))) +
  geom_inset(pies, width = 0.03, height = 0.03, hjust = 0.1) +
  labs(color = "div rate") +
  ggnewscale::new_scale_color() +
  geom_tippoint(size = 1.8, position = position_nudge(x = 1, y = 0.5)) +
  scale_fill_manual(values = "black") +
  ggnewscale::new_scale_color() +
  geom_tippoint(aes(color = as.factor(bibiProc20@data$state)), size = 1, position = position_nudge(x = 1, y = 0.5)) +
  scale_color_manual(values = tipCols) +
  labs(color = "state")
  

# trying it with ape since for whatever reason can't do pies on circular tree

library(ape)

ape::plot.phylo(ladderize(bibiProc20@phylo), show.tip.label = FALSE, type = "fan")
nodelabels(node = as.numeric(bibiNodes20$node),
           pie = as.matrix(cbind(as.numeric(bibiNodes20[,2]), as.numeric(bibiNodes20[,3]))),
           piecol = c("black", "white"), 
           cex = 0.4)

# add a contMap for div time

bibiNodeRates20 <- bibiProc20@data %>% 
  slice(114:n()) %>% 
  select(id, net.div) %>%
  column_to_rownames(var = "id") %>% 
  as.data.frame() 

### fuck it just runs an ape ACE anyway
rateMap <- contMap(bibiProc20@phylo, bibiNodeRates20, plot = TRUE)
















