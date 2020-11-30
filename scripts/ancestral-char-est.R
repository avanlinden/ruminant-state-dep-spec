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

### even rates ace with bibi tree

char <- as.factor(bibiData$sd20)

names(char) <- bibiData$species


aceBibi20_ER <- ace(char, bibiTree, type = "discrete", method = "ML", marginal = TRUE)


plot(bibiTree, cex = 0.6, lwd = 1, label.offset = 1.3, main = "Single-Rate Discrete ACE")
nodelabels(node=1:bibiTree$Nnode+Ntip(bibiTree),
           pie = aceBibi20_ER$lik.anc, piecol = c("black", "white"), cex = 0.4)
tiplabels(pch = 21, col = "black", bg = tipViz[match(bibiTree$tip.label, rownames(tipViz)), 2], cex = 1.5, offset =0.7)

char

charpal <- c("black", "white")
names(charpal) <- c("dimorphic", "monomorphic")

charColors <- charpal[charpal = char]

tipViz <- cbind(char, charColors) 

tipViz[,2]
