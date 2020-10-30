#ancestral state reconstruction

#discrete models

#even rates
aceChen20_ER <- ace(x = chenSize$sd20, phy = chenTree, type = "discrete", marginal = TRUE)

library(ggtree)

## visualize ace models

pal <- c("#F8766D", "#00BFC4")
names(pal) 

plot(chenTree, cex = 0.7, lwd = 1, label.offset = 1.3, main = "Single-Rate Discrete ACE")
nodelabels(node=1:chenTree$Nnode+Ntip(chenTree),
           pie = aceChen20_ER$lik.anc, piecol = c("black", "white"), cex = 0.5)
tiplabels(pch = 15, col = )

