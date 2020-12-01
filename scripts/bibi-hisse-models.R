
### 10 different versions of SSE models from the `hisse` package 
### dull null; cid4; HiSSE; CID2; CID4; plus all of those again with equal transition rates between all states
### HiSSE, CID2, and CID4 all have equal transition rates for all hidden states
### Tree: bibi et al. 2019 WGS ruminant tree
### Sexual dimorphism ratio: 0.2

### Setup ================

library(hisse)
library(diversitree)
library(ape)
library(phangorn)
library(tidyverse)
library(here)

# read in bibi tree and data

tree <- read.tree(here("trees/tidy-bibi-tree.tre"))

data <- read_csv(here("tidy-data/bibi-body-mass-data-sdratio.csv"))

# extract 0.2 and 0.1 threshold data as binary data frames

sd20data <- data %>% 
  mutate(sd20_bin = if_else(sd20 == "dimorphic", 1, 0)) %>% 
  select(species, sd20_bin) %>% 
  as.data.frame()

sd10data <- data %>% 
  mutate(sd10_bin = if_else(sd10 == "dimorphic", 1, 0)) %>% 
  select(species, sd10_bin) %>% 
  as.data.frame()

### Null models ===================

# Two obs states 0 or 1, no hidden states, one div rate, two trans rates

turnover.null <- c(1,1)
ef.null <- c(1,1)
f.null <- c(1,1)
trans.rates.null <- TransMatMakerHiSSE(hidden.traits=0)

null_sd20 <-
  hisse(
    phy = tree,
    data = sd20data,
    f = f.null,
    turnover = turnover.null,
    eps = ef.null,
    hidden.states = FALSE,
    trans.rate = trans.rates.null,
    sann = FALSE
  )

null_sd10 <-
  hisse(
    phy = tree,
    data = sd10data,
    f = f.null,
    turnover = turnover.null,
    eps = ef.null,
    hidden.states = FALSE,
    trans.rate = trans.rates.null,
    sann = FALSE
  )

saveRDS(null_sd20, here("hisse-output/bibi-sd20-null.rds"))

saveRDS(null_sd10, here("hisse-output/bibi-sd10-null.rds"))

# Null model with states 0 or 1, one div rate, and one trans rate between states

eq.trans.rates.null <- ParEqual(trans.rates.null, c(1,2))

eq_null_sd20 <-
  hisse(
    phy = tree,
    data = sd20data,
    f = f.null,
    turnover = turnover.null,
    eps = ef.null,
    hidden.states = FALSE,
    trans.rate = eq.trans.rates.null,
    sann = FALSE
  )

eq_null_sd10 <-
  hisse(
    phy = tree,
    data = sd10data,
    f = f.null,
    turnover = turnover.null,
    eps = ef.null,
    hidden.states = FALSE,
    trans.rate = eq.trans.rates.null,
    sann = FALSE
  )

saveRDS(eq_null_sd20, here("hisse-output/bibi-sd20-null-equal.rds"))

saveRDS(eq_null_sd10, here("hisse-output/bibi-sd10-null-equal.rds"))

### bisse models ====================

# true bisse model with two observed states 0 and 1, no hidden trates, two div rates, and two transition rates

turnover.bisse <- c(1,2)
ef.bisse <- c(1,1)
f.bisse <- c(1,1)
trans.rates.bisse <- TransMatMakerHiSSE(hidden.traits=0)

bisse_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.bisse,
  turnover = turnover.bisse,
  eps = ef.bisse,
  hidden.states = FALSE,
  trans.rate = trans.rates.bisse,
  sann = FALSE
)

bisse_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.bisse,
  turnover = turnover.bisse,
  eps = ef.bisse,
  hidden.states = FALSE,
  trans.rate = trans.rates.bisse,
  sann = FALSE
)

saveRDS(bisse_sd20, here("hisse-output/bibi-sd20-bisse.rds"))

saveRDS(bisse_sd10, here("hisse-output/bibi-sd10-bisse.rds"))

# bisse model with equal transition rates

eq.trans.rates.bisse <- ParEqual(trans.rates.bisse, c(1,2))

eq_bisse_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.bisse,
  turnover = turnover.bisse,
  eps = ef.bisse,
  hidden.states = FALSE,
  trans.rate = eq.trans.rates.bisse,
  sann = FALSE
)

eq_bisse_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.bisse,
  turnover = turnover.bisse,
  eps = ef.bisse,
  hidden.states = FALSE,
  trans.rate = eq.trans.rates.bisse,
  sann = FALSE
)

saveRDS(eq_bisse_sd20, here("hisse-output/bibi-sd20-bisse-equal.rds"))

saveRDS(eq_bisse_sd10, here("hisse-output/bibi-sd10-bisse-equal.rds"))

### HiSSE models ================

# HiSSE model with two observed states 0 and 1, two hidden states A and B, four divergence rates, and five transition rates (trans rates equal between hidden states)

turnover.hisse <- c(1,2,3,4)
ef.hisse <- c(1,1,1,1)
f.hisse = c(1,1)
trans.rates.hisse <- TransMatMakerHiSSE(hidden.traits = 1)

hisse_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.hisse,
  turnover = turnover.hisse,
  eps = ef.hisse,
  hidden.states = TRUE,
  trans.rate = trans.rates.hisse,
  sann = FALSE
)

hisse_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.hisse,
  turnover = turnover.hisse,
  eps = ef.hisse,
  hidden.states = TRUE,
  trans.rate = trans.rates.hisse,
  sann = FALSE
)

saveRDS(hisse_sd20, here("hisse-output/bibi-sd20-hisse.rds"))

saveRDS(hisse_sd10, here("hisse-output/bibi-sd10-hisse.rds"))

# HiSSE model with equal transition rates between all states, hidden and non-hidden

eq.trans.rates.hisse <- ParEqual(trans.rates.hisse, c(1,2,1,3,1,4,1,5))

eq_hisse_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.hisse,
  turnover = turnover.hisse,
  eps = ef.hisse,
  hidden.states = TRUE,
  trans.rate = eq.trans.rates.hisse,
  sann = FALSE
)

eq_hisse_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.hisse,
  turnover = turnover.hisse,
  eps = ef.hisse,
  hidden.states = TRUE,
  trans.rate = eq.trans.rates.hisse,
  sann = FALSE
)

saveRDS(eq_hisse_sd20, here("hisse-output/bibi-sd20-hisse-equal.rds"))

saveRDS(eq_hisse_sd10, here("hisse-output/bibi-sd10-hisse-equal.rds"))

### CID 2 models =================

# CID2: Character-independent diversification model with two rate shifts; two obs states and two hidden states; and 3 transition rates (transition rates equal between hidden states)

turnover.cid2 <- c(1, 1, 2, 2)
ef.cid2 <- c(1,1,1,1)
f.cid2 = c(1,1)
trans.rates.cid2 <- TransMatMakerHiSSE(hidden.traits=1, make.null=TRUE)

cid2_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.cid2,
  turnover = turnover.cid2,
  eps = ef.cid2,
  hidden.states = TRUE,
  trans.rate = trans.rates.cid2,
  sann = FALSE
)

cid2_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.cid2,
  turnover = turnover.cid2,
  eps = ef.cid2,
  hidden.states = TRUE,
  trans.rate = trans.rates.cid2,
  sann = FALSE
)

saveRDS(cid2_sd20, here("hisse-output/bibi-sd20-cid2.rds"))

saveRDS(cid2_sd10, here("hisse-output/bibi-sd10-cid2.rds"))

# CID2 model with equal transition rates for all states

eq.trans.rates.cid2 <- ParEqual(trans.rates.cid2, c(1,2,1,3))

eq_cid2_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.cid2,
  turnover = turnover.cid2,
  eps = ef.cid2,
  hidden.states = TRUE,
  trans.rate = eq.trans.rates.cid2,
  sann = FALSE
)

eq_cid2_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.cid2,
  turnover = turnover.cid2,
  eps = ef.cid2,
  hidden.states = TRUE,
  trans.rate = eq.trans.rates.cid2,
  sann = FALSE
)

saveRDS(eq_cid2_sd20, here("hisse-output/bibi-sd20-cid2-equal.rds"))

saveRDS(eq_cid2_sd10, here("hisse-output/bibi-sd10-cid2-equal.rds"))

### CID4 models =================

# Character independent diversification model with 4 rate shifts; three hidden traits/4 hidden states; 4 divergence rates, and 3 transition rates

turnover.cid4 <- c(1, 1, 2, 2, 3, 3, 4, 4)
ef.cid4 <- rep(1, 8)
f.cid4 <- c(1,1)
trans.rates.cid4 <- TransMatMakerHiSSE(hidden.traits=3, make.null=TRUE)

cid4_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.cid4,
  turnover = turnover.cid4,
  eps = ef.cid4,
  hidden.states = TRUE,
  trans.rate = trans.rates.cid4,
  sann = FALSE
)

cid4_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.cid4,
  turnover = turnover.cid4,
  eps = ef.cid4,
  hidden.states = TRUE,
  trans.rate = trans.rates.cid4,
  sann = FALSE
)

saveRDS(cid4_sd20, here("hisse-output/bibi-sd20-cid4.rds"))

saveRDS(cid4_sd10, here("hisse-output/bibi-sd10-cid4.rds"))

# CID4 model with equal transition rates for all states

eq.trans.rates.cid4 <- ParEqual(trans.rates.cid4, c(1,2,1,3))

eq_cid4_sd20 <- hisse(
  phy = tree,
  data = sd20data,
  f = f.cid4,
  turnover = turnover.cid4,
  eps = ef.cid4,
  hidden.states = TRUE,
  trans.rate = eq.trans.rates.cid4,
  sann = FALSE
)


eq_cid4_sd10 <- hisse(
  phy = tree,
  data = sd10data,
  f = f.cid4,
  turnover = turnover.cid4,
  eps = ef.cid4,
  hidden.states = TRUE,
  trans.rate = eq.trans.rates.cid4,
  sann = FALSE
)

saveRDS(eq_cid4_sd20, here("hisse-output/bibi-sd20-cid4-equal.rds"))

saveRDS(eq_cid4_sd10, here("hisse-output/bibi-sd10-cid4-equal.rds"))

### All model results to list ===================

# function to find all objects of class "hisse.fit" in environment

ClassFilter <- function(x) inherits(get(x), 'hisse.fit' )
hisseObj <- Filter( ClassFilter, ls() )

hisseList <- mget(hisseObj)

### Extract model params =========

AIC.vector <- sapply(hisseList, "[[", "AIC")
AICc.vector <- sapply(hisseList, "[[", "AICc")

### Compute Model Weights ============

AICweights<- GetAICWeights(hisseList, criterion = "AIC")

corrWeights <- GetAICWeights(hisseList, criterion = "AICc")

### Combine params and weights =============

bibiHisseResults <- cbind(hisseObj, AIC.vector, AICc.vector, AICweights, corrWeights) %>% 
  as_tibble() %>% 
  transmute(model = hisseObj, AIC = as.numeric(AIC.vector), AICc = as.numeric(AICc.vector), AICweights = as.numeric(AICweights), AICcorrWeights = as.numeric(corrWeights)) %>% 
  mutate(deltaAIC = AIC - min(AIC)) %>%
  mutate(deltaAICc = AICc - min(AICc)) %>% 
  mutate(
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "cid4") ~ "cid4",
      str_detect(model, "hisse") ~ "hisse",
      str_detect(model, "cid2") ~ "cid2",
      TRUE ~ "cid4")
  ) %>% 
  mutate(sdratio = case_when(
    str_detect(model, "sd20") ~ "0.2",
    str_detect(model, "sd15") ~ "0.15",
    str_detect(model, "sd10") ~ "0.10",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(specRates = case_when(
    str_detect(model, "eq") ~ "equal",
    TRUE ~ "vary")
  )

write_csv(bibiHisseResults, here("tidy-data/bibi-hisse-model-weights.csv"))

bibiHisseResults %>% 
  group_by(specRates) %>% 
  summarise(sumCorrWeights = sum(AICcorrWeights)) %>% 
  arrange(desc(sumCorrWeights))

bibiHisseResults %>% 
  arrange(deltaAICc)

### Average model parameters =================

# Marginal reconstruction of ancestral states for all models
# f is the same for all models here

null_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = null_sd20$solution,
    hidden.states = 1, 
    aic = null_sd20$AICc
  )

null_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = null_sd10$solution,
    hidden.states = 1, 
    aic = null_sd10$AICc
  )

eq_null_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = eq_null_sd20$solution,
    hidden.states = 1, 
    aic = eq_null_sd20$AICc
  )

eq_null_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = eq_null_sd10$solution,
    hidden.states = 1, 
    aic = eq_null_sd10$AICc
  )

bisse_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = bisse_sd20$solution,
    hidden.states = 1,
    aic = bisse_sd20$AICc
  )

eq_bisse_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = eq_bisse_sd20$solution,
    hidden.states = 1,
    aic = eq_bisse_sd20$AICc
  )

bisse_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = bisse_sd10$solution,
    hidden.states = 1,
    aic = bisse_sd10$AICc
  )

eq_bisse_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = eq_bisse_sd10$solution,
    hidden.states = 1,
    aic = eq_bisse_sd10$AICc
  )

hisse_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = hisse_sd20$solution,
    hidden.states = 2,
    aic = hisse_sd20$AICc
  )

eq_hisse_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = eq_hisse_sd20$solution,
    hidden.states = 2,
    aic = eq_hisse_sd20$AICc
  )

hisse_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = hisse_sd10$solution,
    hidden.states = 2,
    aic = hisse_sd10$AICc
  )

eq_hisse_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = eq_hisse_sd10$solution,
    hidden.states = 2,
    aic = eq_hisse_sd10$AICc
  )

cid2_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = cid2_sd20$solution,
    hidden.states = 2,
    aic = cid2_sd20$AICc
  )

eq_cid2_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = eq_cid2_sd20$solution,
    hidden.states = 2,
    aic = eq_cid2_sd20$AICc
  )

cid2_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = cid2_sd10$solution,
    hidden.states = 2,
    aic = cid2_sd10$AICc
  )

eq_cid2_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = eq_cid2_sd10$solution,
    hidden.states = 2,
    aic = eq_cid2_sd10$AICc
  )

cid4_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = cid4_sd20$solution,
    hidden.states = 4,
    aic = cid4_sd20$AICc
  )

eq_cid4_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = eq_cid4_sd20$solution,
    hidden.states = 4,
    aic = eq_cid4_sd20$AICc
  )

cid4_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = cid4_sd10$solution,
    hidden.states = 4,
    aic = cid4_sd10$AICc
  )

eq_cid4_sd10_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd10data,
    f = f.null,
    pars = eq_cid4_sd10$solution,
    hidden.states = 4,
    aic = eq_cid4_sd10$AICc
  )

### save marginal reconstruction objects =========

saveRDS(null_sd20_rec, here("hisse-marginal-recons/bibi-null-sd20.rds"))
saveRDS(eq_null_sd20_rec, here("hisse-marginal-recons/bibi-null-sd20-equal.rds"))
saveRDS(null_sd10_rec, here("hisse-marginal-recons/bibi-null-sd10.rds"))
saveRDS(eq_null_sd10_rec, here("hisse-marginal-recons/bibi-null-sd10-equal.rds"))

saveRDS(cid4_sd20_rec, here("hisse-marginal-recons/bibi-cid4-sd20.rds"))
saveRDS(eq_cid4_sd20_rec, here("hisse-marginal-recons/bibi-cid4-sd20-equal.rds"))
saveRDS(cid4_sd10_rec, here("hisse-marginal-recons/bibi-cid4-sd10.rds"))
saveRDS(eq_cid4_sd10_rec, here("hisse-marginal-recons/bibi-cid4-sd10-equal.rds"))

saveRDS(hisse_sd20_rec, here("hisse-marginal-recons/bibi-hisse-sd20.rds"))
saveRDS(eq_hisse_sd20_rec, here("hisse-marginal-recons/bibi-hisse-sd20-equal.rds"))
saveRDS(hisse_sd10_rec, here("hisse-marginal-recons/bibi-hisse-sd10.rds"))
saveRDS(eq_hisse_sd10_rec, here("hisse-marginal-recons/bibi-hisse-sd10-equal.rds"))

saveRDS(cid2_sd20_rec, here("hisse-marginal-recons/bibi-cid2-sd20.rds"))
saveRDS(eq_cid2_sd20_rec, here("hisse-marginal-recons/bibi-cid2-sd20-equal.rds"))
saveRDS(cid2_sd10_rec, here("hisse-marginal-recons/bibi-cid2-sd10.rds"))
saveRDS(eq_cid2_sd10_rec, here("hisse-marginal-recons/bibi-cid2-sd10-equal.rds"))

saveRDS(cid4_sd20_rec, here("hisse-marginal-recons/bibi-cid4-sd20.rds"))
saveRDS(eq_cid4_sd20_rec, here("hisse-marginal-recons/bibi-cid4-sd20-equal.rds"))
saveRDS(cid4_sd10_rec, here("hisse-marginal-recons/bibi-cid4-sd10.rds"))
saveRDS(eq_cid4_sd10_rec, here("hisse-marginal-recons/bibi-cid4-sd10-equal.rds"))

### Marginal recons to list ===========

ClassFilter <- function(x) inherits(get(x), 'hisse.states' )
hisseReconObj <- Filter( ClassFilter, ls() )

hisseReconList <- mget(hisseReconObj)


### Plot results ===============

plot.hisse.states(hisseReconList, rate.param = "net.div", type = "phylogram", fsize = 0.8, width.factor = 0.4, legend = "none")

### Marginal recons to list ===========

ClassFilter <- function(x) inherits(get(x), 'hisse.states' )
hisseReconObj <- Filter( ClassFilter, ls() )

hisseReconList <- mget(hisseReconObj)


### Plot results ===============

plot.hisse.states(hisseReconList, rate.param = "net.div", type = "phylogram", fsize = 0.8, width.factor = 0.4, legend = "none")

