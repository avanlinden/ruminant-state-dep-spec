### import ruminant body mass data and tidy

# read in body mass data and rename columns

data <- read_csv(here("raw-data/ruminant-size-data.csv")) %>% 
  select("species" = 1, "bibi" = 2, "mkg" = 3, "fkg" = 4)


# join chen tips

tipData <- data %>% 
  full_join(chenNames, by = "species") %>% 
  mutate(chen = ifelse(is.na(common), NA, species)) %>% 
  dplyr::filter(!str_detect(species, "Orcinus orca"))
 
#relabel chen tree with actual names

chenTree$tip.label <- chenNames$species

plot(chenTree)

#drop killer whale 

chenTree <- drop.tip(chenTree, "Orcinus orca")

# separate chen data

chenData <- tipData %>% 
  dplyr::filter(!(is.na(chen))) %>% 
  select(-bibi, -common, -chen)

# names from Chen tree that I don't have size data for:

missing <- chenData %>% 
  dplyr::filter(is.na(mkg) | is.na(fkg)) %>% 
  select(species, mkg, fkg)

missing$mkg <- c(204.2, 13, 102, 30)

missing$fkg <- c(125, 14.7, 60.2, 20)

missing

##rejoin missing data

chenSize <- chenData %>% 
  left_join(missing, by = "species") %>% 
  mutate(mkg = coalesce(mkg.x, mkg.y),
         fkg = coalesce(fkg.x, fkg.y)) %>% 
  select(species, mkg, fkg) %>%
  rowwise() %>% 
  mutate(sdratio = (mkg - fkg)/mean(c(mkg, fkg))) %>% 
  mutate(sd20 = ifelse(sdratio >= 0.2, "dimorphic", "monomorphic"),
       sd15 = ifelse(sdratio >= 0.15, "dimorphic", "monomorphic"),
       sd10 = ifelse(sdratio >= 0.1, "dimorphic", "monomorphic"))


#ancestral state reconstruction

#discrete models

#even rates
aceChen20_ER <- ace(x = chenSize$sd20, phy = chenTree, type = "discrete", marginal = TRUE)


i



