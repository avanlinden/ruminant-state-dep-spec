### import ruminant body mass data and tidy

# read in body mass data and rename columns

data <- read_csv(here("raw-data/ruminant-size-data.csv")) %>% 
  rename("species" = 1, "bibi" = 2, "mkg" = 3, "fkg" = 4, "sdratio" = 5)

# join chen tips

tipData <- data %>% 
  full_join(chenNames, by = "species") %>% 
  mutate(chen = ifelse(is.na(common), NA, species)) %>% 
  dplyr::filter(!str_detect(species, "Orcinus orca")) %>% 
  mutate(sd20 = ifelse(sdratio >= 0.2, "dimorphic", "monomorphic"),
         sd15 = ifelse(sdratio >= 0.15, "dimorphic", "monomorphic"),
         sd10 = ifelse(sdratio >= 0.1, "dimorphic", "monomorphic"))

#relabel chen tree with actual names

chenTree$tip.label <- chenNames$species

plot(chenTree)

#drop killer whale 

chenTree <- drop.tip(chenTree, "Orcinus orca")

# separate chen data

chenData <- tipData %>% 
  dplyr::filter(!(is.na(chen))) %>% 
  select(-bibi, -common, -chen)

chenData$sd20

#ancestral state reconstruction

aceChen20 <- ace(x = chenData$sd20, phy = chenTree, type = "discrete", marginal = TRUE)
