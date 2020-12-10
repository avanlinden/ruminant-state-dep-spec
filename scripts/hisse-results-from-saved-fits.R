
library(tidyverse)
library(hisse)

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

chensd20List <-  chenModelObjList[chenModelNames[str_detect(chenModelNames, "sd20")]]
chensd10List <- chenModelObjList[chenModelNames[str_detect(chenModelNames, "sd10")]]

### Read in bibi model fits ============
# list all files in output directory 
filePaths <- list.files(here("hisse-output"), full.names = TRUE)

#select files with full paths for all chen models
bibiModelPaths <- filePaths[str_detect(filePaths, "bibi")]

#separate list of files without full paths fornaming
files <- list.files(here("hisse-output"))

#create vector of names for models
bibiModelNames <- files[str_detect(files, "bibi")] %>% 
  str_remove(".rds")

#read in all chen tree RDS objects in directory 
bibiModelObjList <- bibiModelPaths %>% 
  map(readRDS)

#store as a named list
names(bibiModelObjList) <- bibiModelNames

#separate based on sdratio

bibisd20List <-  bibiModelObjList[bibiModelNames[str_detect(bibiModelNames, "sd20")]]
bibisd10List <- bibiModelObjList[bibiModelNames[str_detect(bibiModelNames, "sd10")]]




### Extract model AIC =========

AICc20 <- sapply(bibisd20List, "[[", "AICc")
AICc10 <- sapply(bibisd10List, "[[", "AICc")

### Compute Model Weights ============

corrWeights20 <- GetAICWeights(bibisd20List, criterion = "AICc")

corrWeights10 <- GetAICWeights(bibisd10List, criterion = "AICc")

### Combine AIC and weights =============

bibisd20Results <- cbind(model = names(bibisd20List), AICc = as.numeric(AICc20), AICcWeights = as.numeric(corrWeights20)) %>% 
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

write_csv(bibisd20Results, here("tidy-data/bibi-hisse-model-weights-sd20-only.csv"))

bibisd10Results <- cbind(model = names(bibisd10List), AICc = as.numeric(AICc10), AICcWeights = as.numeric(corrWeights10)) %>% 
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

write_csv(bibisd10Results, here("tidy-data/bibi-hisse-model-weights-sd10-only.csv"))



### Extract parameter estimates from each model ===============


chen_sd20_params <- map_dfr(chensd20List, "solution", .id = "model") %>%
  select(model, contains("turnover"), contains("eps0A"), contains("q")) %>%
  mutate(
    tree = "chen",
    sdratio = 0.2,
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4"
    )
  ) %>%
  mutate(across(starts_with("turnover"), ~na_if(.x, 0))) %>% 
  mutate(across(starts_with("q"), ~na_if(.x, 0))) %>% 
  pivot_longer(turnover0A:q1D1C, names_to = "rateParam", values_to = "rateEstimate")
  
   
chen_sd10_params <- map_dfr(chensd10List, "solution", .id = "model") %>%
  select(model, contains("turnover"), contains("eps0A"), contains("q")) %>%
  mutate(
    tree = "chen",
    sdratio = 0.1,
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4"
    )
  ) %>%
  mutate(across(starts_with("turnover"), ~na_if(.x, 0))) %>% 
  mutate(across(starts_with("q"), ~na_if(.x, 0))) %>% 
  pivot_longer(turnover0A:q1D1C, names_to = "rateParam", values_to = "rateEstimate")


bibi_sd20_params <- map_dfr(bibisd20List, "solution", .id = "model") %>%
  select(model, contains("turnover"), contains("eps0A"), contains("q")) %>%
  mutate(
    tree = "bibi",
    sdratio = 0.2,
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4"
    )
  ) %>%
  mutate(across(starts_with("turnover"), ~na_if(.x, 0))) %>% 
  mutate(across(starts_with("q"), ~na_if(.x, 0))) %>% 
  pivot_longer(turnover0A:q1D1C, names_to = "rateParam", values_to = "rateEstimate")



bibi_sd10_params <- map_dfr(bibisd10List, "solution", .id = "model") %>%
  select(model, contains("turnover"), contains("eps0A"), contains("q")) %>%
  mutate(
    tree = "bibi",
    sdratio = 0.1,
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4"
    )
  ) %>%
  mutate(across(starts_with("turnover"), ~na_if(.x, 0))) %>% 
  mutate(across(starts_with("q"), ~na_if(.x, 0))) %>% 
  pivot_longer(turnover0A:q1D1C, names_to = "rateParam", values_to = "rateEstimate")

#allRateParams <- rbind(chen_sd20_params, chen_sd10_params, bibi_sd20_params, bibi_sd10_params)

longRateParams <- rbind(chen_sd20_params, chen_sd10_params, bibi_sd20_params, bibi_sd10_params)

write_csv(longRateParams, here("tidy-data/long-model-fit-rate-params.csv"))


bibi_20_top_params_cid4_eq <- longRateParams %>% 
  dplyr::filter(tree == "bibi" & sdratio == 0.2 & modelType == "cid4" & str_detect(model, "equal")) %>% 
  pivot_wider(names_from = rateParam, values_from = rateEstimate)

bibi_10_top_params_cid4_eq <- longRateParams %>% 
  dplyr::filter(tree == "bibi" & sdratio == 0.1 & modelType == "cid4" & str_detect(model, "equal")) %>% 
  pivot_wider(names_from = rateParam, values_from = rateEstimate)

chen_20_top_params_bisse_vary <- longRateParams %>% 
  dplyr::filter(tree == "chen" & sdratio == 0.2 & modelType == "bisse" & !str_detect(model, "equal")) %>% 
  pivot_wider(names_from = rateParam, values_from = rateEstimate)

chen_20_top_params_bisse_eq <- longRateParams %>% 
  dplyr::filter(tree == "chen" & sdratio == 0.2 & modelType == "bisse" & str_detect(model, "equal")) %>% 
  pivot_wider(names_from = rateParam, values_from = rateEstimate)

chen_10_top_params_bisse_eq <-longRateParams %>% 
  dplyr::filter(tree == "chen" & sdratio == 0.1 & modelType == "bisse" & str_detect(model, "equal")) %>% 
  pivot_wider(names_from = rateParam, values_from = rateEstimate)

chen_10_top_params_bisse_vary <-longRateParams %>% 
  dplyr::filter(tree == "chen" & sdratio == 0.1 & modelType == "bisse" & !str_detect(model, "equal")) %>% 
  pivot_wider(names_from = rateParam, values_from = rateEstimate)

topParamEst <- rbind(bibi_20_top_params_cid4_eq, bibi_10_top_params_cid4_eq, chen_20_top_params_bisse_eq, chen_20_top_params_bisse_vary, chen_10_top_params_bisse_vary, chen_20_top_params_bisse_eq)

write_csv(topParamEst, here("tidy-data/top-model-parameter-estimates.csv"))
