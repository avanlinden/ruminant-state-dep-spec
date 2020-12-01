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

sd20RecList <-  chenRecObjList[chenRecNames[str_detect(chenRecNames, "sd20")]]
sd10RecList <- chenRecObjList[chenRecNames[str_detect(chenRecNames, "sd10")]]

### Plot hisse states ===================

#model average sd20

pdf(file = here("figures/chen-hisse-states-model-avg-sd20.pdf"), width = 8.5, height = 11)

plot.hisse.states(sd20RecList, rate.param = "net.div", type = "phylogram", fsize = 0.6, width.factor = 0.4, legend.cex = 0.6)

dev.off()

#model average sd10

pdf(file = here("figures/chen-hisse-states-model-avg-sd10.pdf"), width = 8.5, height = 11)

plot.hisse.states(sd10RecList, rate.param = "net.div", type = "phylogram", fsize = 0.6, width.factor = 0.4, legend.cex = 0.6)

dev.off()
