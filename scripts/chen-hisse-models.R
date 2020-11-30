
### 10 different versions of SSE models from the `hisse` package 
### dull null; BiSSE; HiSSE; CID2; CID4; plus all of those again with equal transition rates between all states
### HiSSE, CID2, and CID4 all have equal transition rates for all hidden states
### Tree: Chen et al. 2019 WGS ruminant tree
### Sexual dimorphism ratio: 0.2

### Setup ================

library(hisse)
library(diversitree)
library(ape)
library(phangorn)
library(tidyverse)
library(here)

tree <- read.tree(here("trees/tidy-chen-tree.tre"))

data <- read_csv(here("tidy-data/chen-body-mass-data-sdratio.csv"))

sd20data <- data %>% 
  mutate(species = str_replace(species, " ", "_")) %>% 
  mutate(sd20_bin = if_else(sd20 == "dimorphic", 1, 0)) %>% 
  select(species, sd20_bin) %>% 
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

### BiSSE models ====================

# true BiSSE model with two observed states 0 and 1, no hidden trates, two div rates, and two transition rates

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

# BiSSE model with equal transition rates

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

### All model results to list ===================

hisseResults <- list(null_sd20, bisse_sd20, hisse_sd20, cid2_sd20, cid4_sd20)

equalRatesResults <- list(eq_null_sd20, eq_bisse_sd20, eq_hisse_sd20, eq_cid2_sd20, eq_cid4_sd20)

fullResults <- list(null_sd20, bisse_sd20, hisse_sd20, cid2_sd20, cid4_sd20, eq_null_sd20, eq_bisse_sd20, eq_hisse_sd20, eq_cid2_sd20, eq_cid4_sd20)

### Extract model params =========

AIC.vector <- sapply(fullResults, "[[", "AIC")
AICc.vector <- sapply(fullResults, "[[", "AICc")

### Compute Model Weights ============

GetAICWeights(hisseResults, criterion = "AIC")

fullWeights <- GetAICWeights(fullResults, criterion = "AIC")

fullCorrWeights <- GetAICWeights(fullResults, criterion = "AICc")

fullModelNames <- c("null_sd20", "bisse_sd20", "hisse_sd20", "cid2_sd20", "cid4_sd20", "eq_null_sd20", "eq_bisse_sd20", "eq_hisse_sd20", "eq_cid2_sd20", "eq_cid4_sd20")

### Combine params and weights =============

chenHisseResults <- cbind(fullModelNames, AIC.vector, AICc.vector, fullWeights, fullCorrWeights) %>% 
  as_tibble() %>% 
  transmute(model = fullModelNames, AIC = as.numeric(AIC.vector), AICc = as.numeric(AICc.vector), AICweights = as.numeric(fullWeights), AICcorrWeights = as.numeric(fullCorrWeights)) %>% 
  mutate(deltaAIC = AIC - min(AIC)) %>%
  mutate(deltaAICc = AICc - min(AICc)) %>% 
  mutate(
    modelType = case_when(
      str_detect(model, "null") ~ "null",
      str_detect(model, "bisse") ~ "bisse",
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

chenHisseResults %>% 
  group_by(modelType) %>% 
  summarise(sumCorrWeights = sum(AICcorrWeights)) %>% 
  arrange(desc(sumCorrWeights))

chenHisseResults %>% 
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

bisse_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = bisse_sd20$solution,
    hidden.states = 1,
    aic = bisse_sd20$AICc
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

cid2_sd20_rec <-
  MarginReconHiSSE(
    phy = tree,
    data = sd20data,
    f = f.null,
    pars = cid2_sd20$solution,
    hidden.states = 2,
    aic = cid2_sd20$AICc
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

all_recs <- list(null_sd20_rec, bisse_sd20_rec, hisse_sd20_rec, cid2_sd20_rec, cid4_sd20_rec)

### Plot results ===============

plot.hisse.states(all_recs, rate.param = "net.div", type = "phylogram", fsize = 0.8, width.factor = 0.5, legend = "none")

plot.hisse.states(cid4_sd20_rec, rate.param = "turnover", type = "phylogram", fsize = 0.8, legend = "none", width.factor = 0.4)






