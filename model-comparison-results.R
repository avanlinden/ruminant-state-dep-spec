########

# Compare and visualze model likelihoods and parameters

#########


### Load model result files ==============


bibi_sd20 <- read_csv(here("tidy-data/bibi-hisse-model-weights-sd20-only.csv")) %>% 
  mutate(tree = "bibi", sdratio = "20", ntaxa = 117)

bibi_sd10 <- read_csv(here("tidy-data/bibi-hisse-model-weights-sd10-only.csv")) %>% 
  mutate(tree = "bibi", sdratio = "10", ntaxa = 117)

chen_sd20 <- read_csv(here("tidy-data/chen-hisse-model-weights-sd20-only.csv")) %>% 
  mutate(AICcweights = AICcWeights, tree = "chen", sdratio = "20", ntaxa = 51)

chen_sd10 <- read_csv(here("tidy-data/chen-hisse-model-weights-sd10-only.csv")) %>% 
  mutate(AICcweights = AICcWeights, tree = "chen", sdratio = "10", ntaxa = 51)

### Join data tables ===============

allModels <- bind_rows(bibi_sd20, bibi_sd10, chen_sd20, chen_sd10) %>% 
  select(-AICcWeights)

bibi_sd10 %>% 
  group_by(modelType) %>% 
  summarise(wt = sum(AICcweights))

### Make pie graph ================

pieCols <- lacroix_palette("PassionFruit", n = 5, type = "discrete")

print(pieCols)

# pies by model type

allModels %>% 
  mutate(modelType = factor(modelType, levels = c("cid4", "cid2", "hisse", "bisse", "null"))) %>% 
  group_by(tree, sdratio, modelType) %>% 
  summarise(wt = sum(AICcweights), .groups = "drop_last") %>% 
  ggplot(aes(x = "", y = wt, fill = modelType)) +
    geom_bar(width =1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    facet_grid(tree ~ sdratio) +
    scale_fill_manual(values = pieCols) +
     theme_void()

# 

allModels %>% 
  mutate(transRates = factor(transRates)) %>% 
  group_by(tree, sdratio, transRates) %>% 
  summarise(wt = sum(AICcweights), .groups = "drop_last") %>% 
  ggplot(aes(x = "", y = wt, fill = transRates)) +
  geom_bar(width =1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_grid(tree ~ sdratio) +
  scale_fill_manual(values = pieCols) +
  theme_void()


