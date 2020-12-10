########

# Compare and visualze model likelihoods and parameters

#########

library(LaCroixColoR)

### Load model result files ==============


bibi_sd20 <- read_csv(here("tidy-data/bibi-hisse-model-weights-sd20-only.csv")) %>% 
  mutate(tree = "bibi", sdratio = "20", ntaxa = 117)

bibi_sd10 <- read_csv(here("tidy-data/bibi-hisse-model-weights-sd10-only.csv")) %>% 
  mutate(tree = "bibi", sdratio = "10", ntaxa = 117)

chen_sd20 <- read_csv(here("tidy-data/chen-hisse-model-weights-sd20-only.csv")) %>% 
  mutate(tree = "chen", sdratio = "20", ntaxa = 51)

chen_sd10 <- read_csv(here("tidy-data/chen-hisse-model-weights-sd10-only.csv")) %>% 
  mutate(tree = "chen", sdratio = "10", ntaxa = 51)

### Join data tables ===============

allModels <- bind_rows(bibi_sd20, bibi_sd10, chen_sd20, chen_sd10)

allModels

### Make pie graph ================

pieCols <- lacroix_palette("PommeBaya", n = 5, type = "discrete")

lacroix_palettes$Lemon[,c(1:3,5:6)]

wtPies <- lacroix_palettes$PommeBaya[,c(1,5,4,2,3)][1,]

print(pieCols)

# pies by model type

allModels %>% 
  mutate(modelType = factor(modelType, levels = c("cid4", "cid2", "hisse", "bisse", "null"))) %>% 
  group_by(tree, sdratio, modelType) %>% 
  summarise(wt = sum(AICcWeights), .groups = "drop_last") %>% 
  ggplot(aes(x = "", y = wt, fill = modelType)) +
    geom_bar(width =1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    facet_grid(tree ~ sdratio) +
    scale_fill_manual(values = wtPies) +
    theme_void()

ggsave(here("figures/aic-weights-model-type.png"))

# 

allModels %>% 
  mutate(transRates = factor(transRates)) %>% 
  group_by(tree, sdratio, transRates) %>% 
  summarise(wt = sum(AICcWeights), .groups = "drop_last") %>% 
  ggplot(aes(x = "", y = wt, fill = transRates)) +
  geom_bar(width =1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_grid(tree ~ sdratio) +
  scale_fill_manual(values = wtPies) +
  theme_void()

ggsave(here("figures/aic-weights-trans-rates.png"))
