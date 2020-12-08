
### Plotting hisse results with ggtree

library(ggtree)
library(ggimage)
library(gghisse)

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

bibiProc20 <- h_process_recon(bibi_sd20RecList)[[1]]


### plot tree with pies at nodes for states =========

#get nodes likelihoods as df

bibiProc20@data %>% 
  slice(114:n()) 



ggtree(bibiProc20@phylo)
