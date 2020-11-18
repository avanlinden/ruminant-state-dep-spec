# fuck it let's try a HISSE run

### New Hisse Vignette

library(hisse)
library(diversitree)

## simulating practice dataset

suppressWarnings(library(diversitree))
set.seed(4)
# Essentially we are setting up a model that models the evolution of two binary characters
# Thus, we are assuming the following state combinations 1=00, 2=10, 3=01, 4=11:
pars <- c(0.1,0.1,0.1,0.2, rep(0.03, 4), 0.01,0.01,0,0.01,0,0.01,0.01,0,0.01,0,0.01,0.01)
phy <- tree.musse(pars, max.taxa=50, x0=1, include.extinct=FALSE)
sim.dat <- data.frame(names(phy$tip.state), phy$tip.state)
# Now we want to make the states associated with the second character hidden from us. So,
# we remove states 3 and 4 and make them 1 and 2
sim.dat[sim.dat[,2]==3,2] = 1
sim.dat[sim.dat[,2]==4,2] = 2
# This next step simply forces the character to be binary:
sim.dat[,2] = sim.dat[,2] - 1 #0 or 1


### set up dull null model ==========

turnover <- c(1,1)
extinction.fraction <- c(1,1)
f <- c(1,1) #typo in vignette? doesn't run with four values

# set up transition matrix

trans.rates.bisse <- TransMatMakerHiSSE(hidden.traits=0)
print(trans.rates.bisse)
  
  
# estimate null model parameters with default settings

dull.null <- hisse(phy=phy, data=sim.dat, f=f, turnover=turnover, eps=extinction.fraction, hidden.states=FALSE, trans.rate=trans.rates.bisse, sann=FALSE)


### setting up  hisse model ==============

# turnover varies across the four states

turnover <- c(1,2,3,4)
extinction.fraction <- rep(1, 4)
f = c(1,1)

# extend transition rates matrix

trans.rate.hisse <- TransMatMakerHiSSE(hidden.traits=1)
print(trans.rate.hisse)

# call hisse

HiSSE <- hisse(phy=phy, data=sim.dat, f=f, turnover=turnover,
               eps=extinction.fraction, hidden.states=TRUE,
               trans.rate=trans.rate.hisse)

# ok, looking at AIC scores the dull null wins

### setting up CID models ============

# CID 2 -- two rate shifts, one hidden state with make.null

turnover <- c(1, 1, 2, 2)
extinction.fraction <- rep(1, 4)
f = c(1,1)
trans.rate <- TransMatMakerHiSSE(hidden.traits=1, make.null=TRUE)

HiSSE.cid2 <- hisse(phy=phy, data=sim.dat, f=f, turnover=turnover,
               eps=extinction.fraction, hidden.states=TRUE,
               trans.rate=trans.rate.hisse)

# CID 4, three hidden states

turnover <- c(1, 1, 2, 2, 3, 3, 4, 4)
extinction.fraction <- rep(1, 8)
trans.rate <- TransMatMakerHiSSE(hidden.traits=3, make.null=TRUE)


#### try it with  my data ==============

#tree and data

chenTree

chenSD20data <- data %>% 
  mutate(sd20_bin = if_else(sd20 == "dimorphic", 1, 0)) %>% 
  select(species, sd20_bin) %>% 
  as.data.frame()

# null model and trans matrix

turnover <- c(1,1)
extinction.fraction <- c(1,1)
f <- c(1,1)
trans.rates.null <- TransMatMakerHiSSE(hidden.traits=0)

# null model 

chen.null <- hisse(phy=chenTree, data=chenSD20data, f=f, turnover=turnover, eps=extinction.fraction, hidden.states=FALSE, trans.rate=trans.rates.null, sann=FALSE)

# hisse model

turnover <- c(1,2,3,4)
extinction.fraction <- rep(1, 4)
f = c(1,1)

trans.rate.hisse <- TransMatMakerHiSSE(hidden.traits=1)
print(trans.rate.hisse)

HiSSE.equal <- hisse(phy=chenTree, data=chenSD20data, f=f, turnover=turnover,
               eps=extinction.fraction, hidden.states=TRUE,
               trans.rate=trans.rate.hisse.equal)

trans.rate.hisse.equal <- ParEqual(trans.rate.hisse, c(1,2,1,3,1,4,1,5))

chen.HiSSE <- HiSSE

# CID 2 

turnover <- c(1, 1, 2, 2)
extinction.fraction <- rep(1, 4)
f = c(1,1)
trans.rate.cid2 <- TransMatMakerHiSSE(hidden.traits=1, make.null=TRUE)

chen.CID2 <- hisse(phy=chenTree, data=chenSD20data, f=f, turnover=turnover,
                    eps=extinction.fraction, hidden.states=TRUE,
                    trans.rate=trans.rate.cid2)

chen.HiSSE.cid2 <- chen.CID2

# CID 4 - need to read up more on this

turnover <- c(1, 1, 2, 2, 3, 3, 4, 4)
extinction.fraction <- rep(1, 8)
trans.rate.cid4 <- TransMatMakerHiSSE(hidden.traits=3, make.null=TRUE)
f <- c(1,1)

chen.HiSSE.cid4 <- hisse(phy=chenTree, data=chenSD20data, f=f, turnover=turnover,
                         eps=extinction.fraction, hidden.states=TRUE,
                         trans.rate=trans.rate.cid4)

# true BiSSE

turnover <- c(1,2)
extinction.fraction <- c(1,1)
f <- c(1,1)
trans.rates.bisse <- TransMatMakerHiSSE(hidden.traits = 0)

chen.BiSSE   <- hisse(phy = chenTree, data = chenSD20data, f = f, turnover = turnover, eps = extinction.fraction, hidden.states = FALSE, trans.rate = trans.rates.bisse)

# AICc scores
chen.null$AICc
chen.BiSSE$AICc
chen.HiSSE$AICc
chen.HiSSE.cid2$AICc
chen.HiSSE.cid4$AICc

#div time estimates?

str(chen.null$solution)

