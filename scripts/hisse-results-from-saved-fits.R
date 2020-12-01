
### Read in saved hisse.fit model outputs =============

# list all files in output directory 
filePaths <- list.files(here("hisse-output"), full.names = TRUE)

#select files with full paths for all chen models
chenModelPaths <- filePaths[str_detect(filePaths, "chen")]

#separate list of files without full paths fornaming
files <- list.files(here("hisse-output"))

#create vector of names for models
chenModelNames <- files[str_detect(files, "chen")] %>% 
  str_remove(".rds")

#read in all chen tree RDS objects in directory 
chenModelObjList <- chenModelPaths %>% 
  map(readRDS)

#store as a named list
names(chenModelObjList) <- chenModelNames

#separate based on sdratio

sd20List <-  chenModelObjList[chenModelNames[str_detect(chenModelNames, "sd20")]]
sd10List <- chenModelObjList[chenModelNames[str_detect(chenModelNames, "sd10")]]

### Extract model params =========

AICc20 <- sapply(sd20List, "[[", "AICc")
AICc10 <- sapply(sd10List, "[[", "AICc")

### Compute Model Weights ============

corrWeights20 <- GetAICWeights(sd20List, criterion = "AICc")

corrWeights10 <- GetAICWeights(sd10List, criterion = "AICc")

### Combine params and weights =============

sd20Results <- cbind(model = names(sd20List), AICc = as.numeric(AICc20), AICcWeights = as.numeric(corrWeights20)) %>% 
  as_tibble() %>% 
  mutate(AICc = as.numeric(AICc), AICcWeights = as.numeric(AICcWeights), deltaAICc = AICc - min(AICc)) %>% 
  mutate(
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4")
  ) %>% 
  mutate(transRates = case_when(
    str_detect(model, "equal") ~ "equal",
    TRUE ~ "vary")
  )

write_csv(sd20Results, here("tidy-data/chen-hisse-model-weights-sd20-only.csv"))

sd10Results <- cbind(model = names(sd10List), AICc = as.numeric(AICc10), AICcWeights = as.numeric(corrWeights10)) %>% 
  as_tibble() %>% 
  mutate(AICc = as.numeric(AICc), AICcWeights = as.numeric(AICcWeights), deltaAICc = AICc - min(AICc)) %>% 
  mutate(
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4")
  ) %>% 
  mutate(transRates = case_when(
    str_detect(model, "equal") ~ "equal",
    TRUE ~ "vary")
  )

write_csv(sd10Results, here("tidy-data/chen-hisse-model-weights-sd10-only.csv"))
