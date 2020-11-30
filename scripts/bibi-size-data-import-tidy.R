### import ruminant body mass data and tidy

# read in body mass data and rename columns

data <- read_csv(here("raw-data/ruminant-size-data-bibi-compatible.csv")) %>% 
  select("species" = 1, "bibiOriginal" = 2, "mkg" = 3, "fkg" = 4) %>% 
  dplyr::filter(!is.na(mkg)) %>% 
  mutate(bibiSimplified = str_replace(species, " ", "_"))

data 

# read in original bibi tree

fullBibiTree <- read.nexus(here("trees/bibi-mcc.nex"))

fullBibiTips <- as_tibble(fullBibiTree$tip.label)

dropBibi <- fullBibiTips %>% 
  dplyr::filter(!value %in% data$bibiOriginal)

# prune tree to data

bibiTree <- drop.tip(fullBibiTree, dropBibi$value)

plot(bibiTree, cex = 0.6)

bibiTree$tip.label <- data$bibiSimplified

plot(bibiTree, cex = 0.6)

### final Bibi tree ==========

write.nexus(bibiTree, file = here("trees/tidy-Bibi-tree.nex"))

write.tree(bibiTree, file = here("trees/tidy-Bibi-tree.tre"))

### final Bibi data ==========


bibiSizeData  <- data %>% 
  rowwise() %>% 
  mutate(sdratio = (mkg - fkg)/mean(c(mkg, fkg))) %>% 
  select(species = bibiSimplified, bibiOriginal, mkg, fkg, sdratio) %>% 
  mutate(sd20 = ifelse(sdratio >= 0.2, "dimorphic", "monomorphic"),
         sd15 = ifelse(sdratio >= 0.15, "dimorphic", "monomorphic"),
         sd10 = ifelse(sdratio >= 0.1, "dimorphic", "monomorphic"))


write_csv(bibiSizeData, here("tidy-data/bibi-body-mass-data-sdratio.csv"))



