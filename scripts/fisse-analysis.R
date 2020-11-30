
### running a FISSE analysis on sexual dimorphism data using dimorphFISSE wrapper function


# load clean tree and data -- tips must equal data rows

chenTree <- read.tree(here("trees/tidy-chen-tree.tre"))

data <- read_csv(here("tidy-data/chen-body-mass-data-sdratio.csv"))

#adjust species name notation to match tree

data <- mutate(data, species = str_replace(species, " ", "_"))

#source wrapper function
source(here("scripts/ssd-fisse-wrapper-function.R"))

#calculate FISSE p-values for different sexual dimorphism ratio thresholds

chen_sd20 <- as_tibble(dimorphFISSE(chenTree, data, 0.2))

chen_sd15 <- as_tibble(dimorphFISSE(chenTree, data, 0.15))

chen_sd10 <- as_tibble(dimorphFISSE(chenTree, data, 0.1))

# save Fisse objects

saveRDS(chen_sd20, here("fisse-output/chen-fisse-sd20.rds"))

saveRDS(chen_sd15, here("fisse-output/chen-fisse-sd15.rds"))

saveRDS(chen_sd10, here("fisse-output/chen-fisse-sd10.rds"))

### aggregate fisse results ==========

chenFisseResults <- chen_sd20 %>% 
  add_row(chen_sd15) %>% 
  add_row(chen_sd10) %>% 
  mutate(sdratio = c(0.2, 0.15, 0.1)) %>% 
  mutate(pval.2tailed = (1-pval)*2) %>% 
  mutate(tree = "chen-2019", ntaxa = 51, nsim = 10000)

chenFisseResults

write_csv(chenFisseResults, here("tidy-data/chen-fisse-results.csv"))


### FiSSE analysis with Bibi 2013 tree ==================


bibiTree <- read.tree(here("trees/tidy-Bibi-tree.tre"))

bibiData <- read_csv(here("tidy-data/bibi-body-mass-data-sdratio.csv"))

source(here("scripts/ssd-fisse-wrapper-function.R"))

bibi_sd20 <- as_tibble(dimorphFISSE(bibiTree, bibiData, 0.2))

bibi_sd15 <- as_tibble(dimorphFISSE(bibiTree, bibiData, 0.15))

bibi_sd10 <- as_tibble(dimorphFISSE(bibiTree, bibiData, 0.1))

### save bibi result objects

saveRDS(bibi_sd20, here("fisse-output/bibi-fisse-sd20.rds"))

saveRDS(bibi_sd15, here("fisse-output/bibi-fisse-sd15.rds"))

saveRDS(bibi_sd10, here("fisse-output/bibi-fisse-sd10.rds"))

# aggregate bibi results

bibiFisseResults <- bibi_sd20 %>% 
  add_row(bibi_sd15) %>% 
  add_row(bibi_sd10) %>% 
  mutate(sdratio = c(0.2, 0.15, 0.1)) %>% 
  mutate(pval.2tailed = (1-pval)*2) %>% 
  mutate(tree = "bibi-2013", ntaxa = length(bibiTree$tip.label), nsim = 10000)

bibiFisseResults

write_csv(bibiFisseResults, here("tidy-data/bibi-fisse-results.csv"))


combinedFisseResults <- rbind(bibiFisseResults, chenFisseResults)

write_csv(combinedFisseResults, here("tidy-data/combined-fisse-results.csv"))









