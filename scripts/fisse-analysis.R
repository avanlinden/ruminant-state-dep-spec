
### running a FISSE analysis on sexual dimorphism data using dimorphFISSE wrapper function


# load clean tree and data -- tips must equal data rows

chenTree <- read.tree(here("trees/tidy-chen-tree.tre"))

data <- read_csv(here("tidy-data/chen-body-mass-data-sdratio.csv"))

#adjust species name notation to match tree

data <- mutate(data, species = str_replace(species, " ", "_"))

#source wrapper function
source(here("scripts/ssd-fisse-wrapper-function.R"))

#calculate FISSE p-values for different sexual dimorphism ratio thresholds

chen_sd20 <- dimorphFISSE(chenTree, data, 0.2)

chen_sd15 <- dimorphFISSE(chenTree, data, 0.15)

chen_sd10 <- dimorphFISSE(chenTree, data, 0.1)



