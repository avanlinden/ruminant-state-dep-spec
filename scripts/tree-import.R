
# import and inspect trees

### load ruminant whole-genome ML tree from Chen et al 2019 Science =============
### (divergence times added manually from SI, originally estimated using r8s software)

# load tree

library(ape)

chenTree <- read.nexus(here("trees/chen2019-ruminant-WGS-ML-divtimes.nex"))

#check
plot(chenTree)

#extract tip names

write_csv(as.data.frame(chenTree$tip.label), here("raw-data/raw-tip-labels.csv"), quote_escape = "double")

# reimport scientific tip names

chenNames <- read_csv(here("raw-data/sci-names-tip-labels.csv")) %>% 
  rename("common" = 1, "species" = 2)

#relabel chen tree with actual names

chenTree$tip.label <- chenNames$species

plot(chenTree)

#drop killer whale 

chenTree <- drop.tip(chenTree, "Orcinus orca")

## save pruned and renamed tree

write.nexus(chenTree, file = here("trees/tidy-Chen-tree.nex"))

write.tree(chenTree, file = here("trees/tidy-chen-tree.tre"))



#### Did all of the below in excel because synonyms/subspecies were too difficult to deal with ========

# ### Bibi 2013 mitochondrial genome bovid tree 
# 
# fullBibiTree <- read.nexus(here("trees/bibi-mcc.nex"))
# 
# ## collapse subspecies and multiple specimen numbers per tip
# 
# bibiTips <- as_tibble(fullBibiTree$tip.label)
# 
# # bibiCounts <- bibiTips %>% 
# #   separate(value, into = c("genus", "species", "subspecies"), sep = "_", fill = "right") %>% 
# #   mutate(genSpec = str_c(genus, species, sep = "_")) %>% 
# #   count(genSpec, sort = TRUE)
# 
# collapsedBibiTips <- bibiTips %>% 
#   separate(value, into = c("genus", "species", "subspecies", "id"), sep = "_", fill = "right") %>% 
#   mutate(genSpec = str_c(genus, species, sep = "_")) %>% 
#   distinct(genSpec, .keep_all = TRUE) %>% 
#   mutate(bibiName = case_when(
#     is.na(subspecies) ~ genSpec,
#     is.na(id) & !is.na(subspecies) ~ str_c(genSpec, subspecies, sep = "_"),
#     TRUE ~ str_c(genSpec, subspecies, id, sep = "_")
#   )) 
# 
# 
# dropBibiTips <- bibiTips %>% 
#   dplyr::filter(!value %in% collapsedBibiTips$bibiName) 
# 
# 
# # collapse Bibi tree to unique species
# 
# 
# bibiTree <- drop.tip(fullBibiTree, dropBibiTips$value)
# 
# 
# 
# prunedBibiTips <- as_tibble(bibiTree$tip.label)
# 
# 
# prunedBibiGenSpec <- prunedBibiTips %>% 
#   separate(value, into = c("genus", "species", "subspecies", "id"), sep = "_", fill = "right") %>% 
#   mutate(genSpec = str_c(genus, species, sep = "_")) %>% 
#   select(genSpec)
# 
# bibiTree$tip.label <- prunedBibiGenSpec$genSpec
# 
# plot(bibiTree, cex = 0.7)
# 
# #### use this tree for further analyses: ===============
# 
# # bibiTree
# # 
# # write.nexus(bibiTree, file = here("trees/tidy-Bibi-tree.nex"))
# # 
# # write.tree(bibiTree, file = here("trees/tidy-Bibi-tree.tre"))


