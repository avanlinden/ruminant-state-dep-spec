# Script to produce all figures from the article
#  All relevant result files are in the batch_results directory


# bird results
b.01 <- read.csv("birds_nullsims_200-500_q0.01.csv")
b.1 <- read.csv("birds_nullsims_200-500_q0.1.csv")
b1 <- read.csv("birds_nullsims_200-500_q1.csv")
b10 <- read.csv("birds_nullsims_200-500_q10.csv")
basymm <- read.csv("birds_nullsims_100-500_asymmQ.csv")

fx <- function(x) return((sum(x$pval < 0.025 | x$pval > 0.975)) / nrow(x))

t1rates <- numeric(5)

t1rates[1] <- fx(b.01)
t1rates[2] <- fx(b.1)
t1rates[3] <- fx(b1)
t1rates[4] <- fx(b10)
t1rates[5] <- fx(basymm)


lvf <-  "bissesims_lambdavar_w_bissefits.csv"
mvf <-  "bissesims_muvar_w_bissefits.csv"
bvar <- "bissesims_bothvar_rfixed_wBiSSEfits.csv"


lv <- read.csv(paste(lvf, sep=""))
mv <- read.csv(paste( mvf, sep=""))
bv <- read.csv(paste(bvar, sep=""))
names(bv)[1:7] <- names(mv)[1:7]
colnames(bv)[8:9] <- c("lambda0_true", "lambda1_true")

lrt <- (2 * (lv$b4_loglik - lv$b3_loglik))
lrt[lrt < 0] <- 0

lv$lrpval <- 1 - pchisq(lrt, df=1)


lrt <- (2 * (mv$b4_loglik - mv$b3_loglik))
lrt[lrt < 0] <- 0

mv$lrpval <- 1 - pchisq(lrt, df=1)

# For both variable results:

lrt1 <- (2 * (bv$b4_loglik - bv$b3_loglik))
lrt1[lrt1 < 0] <- 0

lrt2 <- (2 * (bv$b5_loglik - bv$b3_loglik))
lrt2[lrt2 < 0] <- 0

p1 <- 1 - pchisq(lrt1, df=1)
p2 <- 1 - pchisq(lrt2, df=2)

# get min pvalue from LRT for m5 v m3 and m4 v m3
bv$lrpval <- apply(cbind(p1,p2), 1, min)

lres <- matrix(NA, nrow=5, ncol=4)
mres <- matrix(NA, nrow=5, ncol=4)
bres <- matrix(NA, nrow=5, ncol=4)
bv$nvec <-  rep(c(100,200,300,400,500), 200)


nvec <- seq(100, 500, by=100)
for (i in 1:length(nvec)){
	lres[i,1] <- sum(lv$nvec == nvec[i] & lv$lrpval < 0.05 & lv$lambda1_true == 0.1)
	lres[i,3] <- sum(lv$nvec == nvec[i] & (lv$pval < 0.025 | lv$pval > 0.975) & lv$lambda1_true == 0.1)
	lres[i,2] <- sum(lv$nvec == nvec[i] & lv$lrpval < 0.05 & lv$lambda1_true == 0.2)
	lres[i,4] <- sum(lv$nvec == nvec[i] & (lv$pval < 0.025 | lv$pval > 0.975) & lv$lambda1_true == 0.2)

	mres[i,1] <- sum(mv$nvec == nvec[i] & mv$lrpval < 0.05 & mv$lambda1_true == 0.1)
	mres[i,3] <- sum(mv$nvec == nvec[i] & (mv$pval < 0.025 | mv$pval > 0.975) & mv$mu1 == 0.1)
	mres[i,2] <- sum(mv$nvec == nvec[i] & mv$lrpval < 0.05 & mv$lambda1_true == 0.2)
	mres[i,4] <- sum(mv$nvec == nvec[i] & (mv$pval < 0.025 | mv$pval > 0.975) & mv$mu1 == 0)

	bres[i,1] <- sum(bv$nvec == nvec[i] & (bv$pval < 0.025 | bv$pval > 0.975) & bv[,9] == 0.2)
	bres[i,2] <- sum(bv$nvec == nvec[i] & (bv$pval < 0.025 | bv$pval > 0.975) & bv[,9] == 0.3)
  	bres[i,3] <- sum(bv$nvec == nvec[i] & bv$lrpval < 0.05 & bv[,9] == 0.2)
  	bres[i,4] <- sum(bv$nvec == nvec[i] & bv$lrpval < 0.05 & bv[,9] == 0.3)


}

bres <- bres / 100
lres <- lres / 100
mres <- mres / 100

#-----------------------------------------------#
# Figure 1


plotSetup <- function(){
	plot.new()
	par(mar=c(6,3,1,1))
	plot.window(xlim=c(50, 550), ylim=c(0,1))
	axis(1, at=seq(0,600, by=100))
	axis(2, at=seq(-0.2,1.2, by=0.2), las=1)
	mtext(side=1, "Number of tips", line=3.5, cex=1.2)
}

pplot <- function(y, pch, bg, cex=1.5){
	xvals <- seq(100, 500, by=100)
	lines(xvals, y, lwd=2, col="gray50")
	points(xvals, y, pch=pch, bg=bg, cex=cex)
}

quartz.options(height=4.5, width=13)
par(oma=c(1,3,2,1))
par(mfrow=c(1,4))
 
plotSetup()
pplot(lres[,1], 23, 'blue', cex=1.8)
pplot(lres[,3], 21, 'red', cex=1.8)

points(x=100, y=0.52, pch=21, bg="red", cex=1.7)
points(x=100, y=0.45, pch=23, bg="blue", cex=1.7)
text(x=110, y=0.52, pos=4, labels="FISSE", cex=1.5)
text(x=110, y=0.44, pos=4, labels="BiSSE", cex=1.5)

mtext(side=2, "Proportion significant", line=3.5, cex=1.2)
mtext("A", side=2, at=1.05, line=3.5, las=1, cex=1.7)
mtext(side=3, text=expression(lambda ~ "= 0.1, " ~ mu ~ "= 0.03"), cex=1, line=0)

plotSetup()
pplot(lres[,2], 23, 'blue', cex=1.8)
pplot(lres[,4], 21, 'red', cex=1.8)
mtext("B", side=2, at=1.05, line=3.5, las=1, cex=1.7)
mtext(side=3, text=expression(lambda[0] ~ "= 0.1, " ~ lambda[1] ~"= 0.2," ~ mu ~ "= 0.03"), cex=1, line=0)


plotSetup()
pplot(mres[,2], 23, 'blue', cex = 1.8)
pplot(mres[,4], 21, 'red', cex = 1.8)
mtext("C", side=2, at=1.05, line=3.5, las=1, cex=1.7)
mtext(side=3, text=expression(lambda ~ "= 0.2, " ~ mu[0] ~"= 0.1," ~ mu[1] ~ "= 0"), cex=1, line=0)



plotSetup()
pplot(bres[,3], 23, 'blue', cex = 1.8)
#pplot(bres[,4], 23, 'blue', cex = 1.8)
pplot(bres[,1], 21, 'red', cex = 1.8)
#pplot(bres[,2], 21, 'red', cex = 1.8)

mtext("D", side=2, at=1.05, line=3.5, las=1, cex=1.7)
mtext(side=3, text=expression("(" ~ lambda[0] ~ " - " ~ mu[0] ~ ") = (" ~ lambda[1] ~ " - " ~ mu[1] ~ ")"), cex=1, line=0)



#-----------------------------------------------#
# Figure 2
# 
# pairwise rate plot1

lv$colvec <- rep("white", nrow(lv))
lv$colvec[lv$lambda1_true == 0.2] <- "black"



quartz.options(height=5, width=9)
par(oma=c(0,0,2,0))
par(mfrow=c(1,2))

plot.new()
par(mar=c(5,5,1,1))

plot.window(xlim=c(0, 0.37), ylim=c(0,0.37), asp=1)

lines(x=c(0.1, 0.1), y=c(0, 0.4), lwd=3,  col="gray50")
lines(x=c(0, 0.4), y=c(0.1, 0.1), lwd=3,  col="gray50")

lines(x=c(0.102, 0.102), y=c(0, 0.4), lwd=3, lty="dotted",  col="gray50")
lines(x=c(0, 0.4), y=c(0.2, 0.2), lwd=3, lty="dotted", col="gray50")


#lines(x=c(0.1, 0.1), y=c(0, 0.4), lwd=2, lty="dotted", col="gray50")
#lines(x=c(0, 0.4), y=c(0.2, 0.2), lwd=2, lty="dotted", col="gray50")


#lines(x=c(0,1), y=c(0,1), lwd=2, col="gray50")
#

points(lv$lambda0, lv$lambda1, pch=21, bg=lv$colvec, cex=0.8)
axis(1, at=seq(-0.1, 0.4, by=0.1))
axis(2, at=seq(-0.1, 0.4, by=0.1), las=1)
mtext(side=1, text = expression(Lambda[0]), line=3, cex=1.5)
mtext(side=2, expression(Lambda[1]), line=3, cex=1.3)
mtext("A", side = 3, at = -0.05, line=0.5, cex=1.7, las=1)
mtext(side=3, text="All trees", line=0.5, cex=1.2)

points(x=0.35, y=0.06,pch=21, bg= "black", cex=1.3)
points(x=0.35, y=0.03,pch=21, bg= "white", cex=1.3)
text(x=0.35, y=0.06, labels="SDD", pos=2, cex=1.3)
text(x=0.35, y=0.027, labels="no SDD", pos=2, cex=1.3)

######

isSig <- lv$pval < 0.025 | lv$pval > 0.975

plot.new()
par(mar=c(5,5,1,1))

plot.window(xlim=c(0, 0.37), ylim=c(0,0.37), asp=1)
 
lines(x=c(0.1, 0.1), y=c(0, 0.4), lwd=3,  col="gray50")
lines(x=c(0, 0.4), y=c(0.1, 0.1), lwd=3,  col="gray50")

lines(x=c(0.102, 0.102), y=c(0, 0.4), lwd=3, lty="dotted",  col="gray50")
lines(x=c(0, 0.4), y=c(0.2, 0.2), lwd=3, lty="dotted", col="gray50")
 
 
#lines(x=c(0,1), y=c(0,1), lwd=2, col="gray50")
#

points(lv$lambda0[isSig], lv$lambda1[isSig], pch=21, bg=lv$colvec[isSig], cex=0.8)
axis(1, at=seq(-0.1, 0.4, by=0.1))
axis(2, at=seq(-0.1, 0.4, by=0.1), las=1)

mtext(side=1, text = expression(Lambda[0]), line=3, cex=1.5)
mtext(side=2, expression(Lambda[1]), line=3, cex=1.3)
mtext("B", side = 3, at = -0.05, line=0.5, cex=1.7, las=1)
mtext(side=3, text="Trees w/ SDD inferred", line=0.5, cex=1.2)
 

##############################
#-----------------------------------------------#
# Figure 3
# 
# pairwise rate plot2

tmp <- bv[bv$lambda1_true == 0.2, ]
tmp2 <- bv[bv$lambda1_true == 0.3, ]

 
quartz.options(height=5, width=9)
par(oma=c(0,0,2,0))
par(mfrow=c(1,2))

plot.new()
par(mar=c(5,5,1,1))

plot.window(xlim=c(0, 0.37), ylim=c(0,0.37), asp=1)
 
lines(x=c(0.1, 0.1), y=c(0, 0.4), lwd=2, lty="dotted", col="gray50")
lines(x=c(0, 0.4), y=c(0.2, 0.2), lwd=2, lty="dotted", col="gray50")

#lines(x=c(0,1), y=c(0,1), lwd=2, col="gray50")
#

points(tmp$lambda0, tmp$lambda1, pch=19, col="black", cex=0.6)
axis(1, at=seq(-0.1, 0.4, by=0.1))
axis(2, at=seq(-0.1, 0.4, by=0.1), las=1)
mtext(side=1, text = expression(Lambda[0]), line=3, cex=1.5)
mtext(side=2, expression(Lambda[1]), line=3, cex=1.3)
mtext("A", side = 3, at = -0.05, line=0.5, cex=1.7, las=1)
mtext(side=3, text=expression(lambda[0] ~ "= 0.1, " ~ lambda[1] ~ "= 0.2, " ~ mu[0] ~"= 0.03," ~ mu[1] ~ "= 0.13"), cex=1, line=0.4)

######

plot.new()
par(mar=c(5,5,1,1))

plot.window(xlim=c(0, 0.37), ylim=c(0,0.37), asp=1)
 
lines(x=c(0.1, 0.1), y=c(0, 0.4), lwd=2, lty="dotted", col="gray50")
lines(x=c(0, 0.4), y=c(0.3, 0.3), lwd=2, lty="dotted", col="gray50")


points(tmp2$lambda0, tmp2$lambda1, pch=19, col="black", cex=0.6)
axis(1, at=seq(-0.1, 0.4, by=0.1))
axis(2, at=seq(-0.1, 0.4, by=0.1), las=1)
mtext(side=1, text = expression(Lambda[0]), line=3, cex=1.5)
mtext(side=2, expression(Lambda[1]), line=3, cex=1.3)
mtext("B", side = 3, at = -0.05, line=0.5, cex=1.7, las=1)
mtext(side=3, text=expression(lambda[0] ~ "= 0.1, " ~ lambda[1] ~ "= 0.3, " ~ mu[0] ~"= 0.03," ~ mu[1] ~ "= 0.23"), cex=1, line=0.4)

#-----------------------------------------------#
# Figure 4
# 
# birds Type I error rates

labvec <- c("q = 0.01", "q = 0.1", "q = 1", "q = 10", "Asymm")

quartz.options(height=5, width=6)
plot.new()
par(oma=c(1,1,1,1))
par(mar=c(6,6,1,1))
plot.window(xlim=c(0.5, 5.5), ylim=c(0,1))
points(1:5, t1rates, pch=21, bg="red", cex=1.3)
axis(1, at=seq(0,5,by=1), labels=c(NA, labvec), cex.axis = 1)
axis(2, at=seq(-0.2, 1, by=0.2), las=1)
mtext(side=2, "Proportion significant", line=3.5, cex=1.2)
mtext(side=1, "Transition rate", line=3.5, cex=1.2)


#########################


##############################
#-----------------------------------------------#
# Figure 5
# 
# BiSSE / FISSE comparison across EEG sim datasets

xx <- read.table("Feb2017_final_setsummaries.csv", stringsAsFactors=F, header=T, sep=",")

xxy <- xx[xx$True.SDD == "yes", ]
xxn <- xx[xx$True.SDD == "no", ]

#xxy <- xxy[order(xxy$bisse.sig, xxy$fisse.sig), ]
#xxn <- xxn[order(xxn$bisse.sig, xxn$fisse.sig), ]


quartz.options(height = 4, width=12)
plot.new()
par(oma=c(1,2,1,1))
mm <- matrix(c(1, 2,2), nrow=2, ncol=3, byrow=T)
layout(mm)

wid <- 0.25

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim=c(0, 16), ylim=c(0,1))


for (i in 1:nrow(xxy)){
	lines(c(i,i), y=c(xxy$bisse.sig[i], xxy$fisse.sig[i]), lwd=0.9, col="gray60")
	points(i, xxy$bisse.sig[i], pch=23, bg="blue", cex=1.8)
	#points(i, xxy$r.frac.sdd[i], pch=24, bg="blue", cex=1.8)
	points(i , xxy$fisse.sig[i], pch=21, bg="red", cex=1.6)	
	
	if (is.na(xxn$original_index_doubleblind[i])){
		mtext("*", side=1, at=i, line=1.1, cex=1.5)
	}
}

points(0.5, 0.9, pch=23, bg="blue", cex=1.8)
points(0.5, 0.8, pch=21, bg="red", cex=1.6)
text(0.7, 0.9, labels="BiSSE", cex=1.5, pos=4, font=3)
text(0.7, 0.8, labels="FiSSE", cex=1.5, pos=4, font=3)

axis(1, at=c(-2, seq(1, 15, by=1), 16), labels=NA)
axis(1, at=seq(1, 15, by=2), tick=F, cex.axis=1.2, padj=0.5)
mtext("Simulation scenario: true SDD", side=1, line=3.25, cex=1.2)

axis(2, at=seq(-0.1, 1.0, by=0.1), label=NA)
axis(2, at=seq(0, 1, by=0.2), las=1, tick=F, cex.axis=1.2)

mtext(side=2, text="Proportion significant", line= 3.7, cex=1.2)
mtext(side=2, text="A", line= 3.7, cex=1.4, las=1, at=1.05)

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim=c(0, 34.5), ylim=c(0,1))
polygon(x=c(-2,-2, 35,35), y=c(0,0.05, 0.05,0), border=F, col="gray80")

for (i in 1:nrow(xxn)){
	lines(c(i,i), y=c(xxn$bisse.sig[i], xxn$fisse.sig[i]), lwd=0.75, col="gray70")
	points(i, xxn$bisse.sig[i], pch=23, bg="blue", cex=1.8)
	#points(i, xxn$r.frac.sdd[i], pch=24, bg="blue", cex=1.8)
	points(i , xxn$fisse.sig[i], pch=21, bg="red", cex=1.6)	
	
	if (is.na(xxn$original_index_doubleblind[i])){
		mtext("*", side=1, at=i, line=1.1, cex=1.5)
	}
 
}

axis(1, at=c(-2, seq(1, 34, by=1), 36), labels=NA)
axis(1, at=seq(1, 33, by=2), labels= seq(17, 49, by=2), tick=F, cex.axis=1.2, padj=0.5)
mtext("Simulation scenario: no SDD", side=1, line=3.25, cex=1.2)
axis(2, at=seq(-0.1, 1.0, by=0.1), label=NA)
axis(2, at=seq(0, 1, by=0.2), las=1, tick=F, cex.axis=1.2)
#mtext(side=2, text="Type I error rate", line= 3.5, cex=1.2)
mtext(side=2, text="Proportion significant", line= 3.7, cex=1.2)
mtext(side=2, text="B", line= 3.7, cex=1.4, las=1, at=1.05)


##############################

##############################
#-----------------------------------------------#
# Figure 6
# 
# HiSSE comparison across EEG sim datasets
 
 

xx <- read.table("Feb2017_final_setsummaries.csv", stringsAsFactors=F, header=T, sep=",")

xxy <- xx[xx$True.SDD == "yes", ]
xxn <- xx[xx$True.SDD == "no", ]
 
quartz.options(height = 4, width=12)
plot.new()
par(oma=c(1,2,1,1))
mm <- matrix(c(1, 2,2), nrow=2, ncol=3, byrow=T)
layout(mm)

wid <- 0.25

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim=c(0, 16), ylim=c(0,1))

for (i in 1:nrow(xxy)){
	lines(c(i,i, i), y=c(xxy$fullset_sdd.frac[i], xxy$cid_bisse.frac[i], xxy$fisse.sig[i]), lwd=0.9, col="gray60")
	points(i, xxy$fullset_sdd.frac[i], pch=22, col="blue", bg="blue", cex=1.9)
 	points(i , xxy$cid_bisse.frac[i], pch=24, col="blue", bg="white", cex=1.6)	
	points(i , xxy$fisse.sig[i], pch=21, bg="red", cex=1.6)	
	
	if (is.na(xxy$original_index_doubleblind[i])){
		mtext("*", side=1, at=i, line=1.1, cex=1.5)
	}
	
}

points(0.5, 1, pch=24, bg="white", col="blue", cex=1.6)

points(0.5, 0.9, pch=22, bg="blue", cex=1.8)
points(0.5, 0.8, pch=21, bg="red", cex=1.6)

text(0.7, 1.0, labels="BiSSE", cex=1.5, pos=4, font=3)
text(0.7, 0.9, labels="BiSSE+HiSSE", cex=1.5, pos=4, font=3)

text(0.7, 0.8, labels="FiSSE", cex=1.5, pos=4, font=3)


axis(1, at=c(-2, seq(1, 15, by=1), 16), labels=NA)
axis(1, at=seq(1, 15, by=2), tick=F, cex.axis=1.2, padj=0.5)
mtext("Simulation scenario: true SDD", side=1, line=3.25, cex=1.2)
axis(2, at=seq(-0.1, 1.0, by=0.1), label=NA)
axis(2, at=seq(0, 1, by=0.2), las=1, tick=F, cex.axis=1.2)
mtext(side=2, text="Proportion significant", line= 3.7, cex=1.2)
mtext(side=2, text="A", line= 3.7, cex=1.4, las=1, at=1.05)

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim=c(0, 34.5), ylim=c(0,1))
polygon(x=c(-2,-2, 35,35), y=c(0,0.05, 0.05,0), border=F, col="gray80")

for (i in 1:nrow(xxn)){
	lines(c(i,i, i), y=c(xxn$fullset_sdd.frac[i], xxn$cid_bisse.frac[i], xxn$fisse.sig[i]), lwd=0.9, col="gray60")
	points(i, xxn$fullset_sdd.frac[i], pch=22, col="blue", bg="blue", cex=1.9)
 	points(i , xxn$cid_bisse.frac[i], pch=24, col="blue", bg="white", cex=1.6)	
	points(i , xxn$fisse.sig[i], pch=21, bg="red", cex=1.6)	
	
	if (is.na(xxn$original_index_doubleblind[i])){
		mtext("*", side=1, at=i, line=1.1, cex=1.5)
	}	
	
}


axis(1, at=c(-2, seq(1, 34, by=1), 36), labels=NA)
axis(1, at=seq(1, 33, by=2), labels= seq(17, 49, by=2), tick=F, cex.axis=1.2, padj=0.5)
mtext("Simulation scenario: no SDD", side=1, line=3.25, cex=1.2)
axis(2, at=seq(-0.1, 1.0, by=0.1), label=NA)
axis(2, at=seq(0, 1, by=0.2), las=1, tick=F, cex.axis=1.2)
#mtext(side=2, text="Type I error rate", line= 3.5, cex=1.2)
mtext(side=2, text="Proportion significant", line= 3.7, cex=1.2)
mtext(side=2, text="B", line= 3.7, cex=1.4, las=1, at=1.05)

#-----------------------------------------------#
# Figure S1 ## Threshold stuff

tt <- read.csv("FiSSE_threshold_analysis.csv", stringsAsFactors=F)
quartz.options(height=6, width=6)
plot.new()
par(oma=c(1,1,1,1))
par(mar=c(5,5,1,1))
plot.window(xlim=c(0,1), ylim=c(0,1), asp=1)
lines(x=c(0,1), y=c(0,1), lwd=5, col="gray70")

points(tt$t0.1, tt$t0.25, pch=21, bg="coral", cex=0.9)
axis(1, at=seq(-0.2, 1, by=0.2))
axis(2, at=seq(-0.2, 1, by=0.2), las=1)
mtext("P-value, threshold = 0.1", side=1, line=3, cex=1.5)
mtext("P-value, threshold = 0.25", side=2, line=3, cex=1.5)


#-----------------------------------------------#
# New Figure S2 ## 

 

fx <- function(x){
	if (x > 0.5){
		x <- 1 - x
	} 
	return(2 * x)
}
lv$p2 <- sapply(lv$pval, fx)
 
true_sdd <- ! abs(lv$lambda0_true - lv$lambda1_true) < 0.001 
 
 
# Plot pvalue (2 tailed) versus parsimony score 
axcex <- 0.8

quartz.options(height=9, width=5)
plot.new()
par(mfrow=c(2,1))
par(oma=c(1,1,1,1))

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim = c(0, 4.3), ylim=c(0, 1))

alabs <-  c(1, 2, 5, 10, 20, 50, 100)
axis_tics <- c(-1, log(alabs))

lines(x=c(0, 4), y=c(0.05, 0.05), lwd=3, col="gray50", lty="dotted")
points(log(lv$nchanges_parsimony[true_sdd  ]), lv$p2[true_sdd  ], pch=21, bg="gray60", cex=0.8)

axis(1, at=axis_tics, labels=c(NA, alabs), cex.axis=axcex)
axis(2, at=seq(-.2, 1, by=0.2), las=1, cex.axis=axcex)

mtext(side = 1, text = "State changes under parsimony", line=3, cex=1.2)
mtext(side = 2, text = "p-value (2 tailed)", line=3, cex=1.2)
mtext("A", side=2, at=1.05, cex=1.5, line=3.5, las=1)

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim = c(0, 4.3), ylim=c(0, 1))

lines(x=c(0, 4), y=c(0.05, 0.05), lwd=3, col="gray50", lty="dotted")
points(log(lv$nchanges_parsimony[!true_sdd]), lv$p2[!true_sdd], pch=21, bg="gray60", cex=0.8)
 
axis(1, at=axis_tics, labels=c(NA, alabs), cex.axis=axcex)
axis(2, at=seq(-.2, 1, by=0.2), las=1, cex.axis=axcex)

mtext(side = 1, text = "State changes under parsimony", line=3, cex=1.2)
mtext(side = 2, text = "p-value (2 tailed)", line=3, cex=1.2)
mtext("B", side=2, at=1.05, cex=1.5, line=3.5, las=1)


##############################
#-----------------------------------------------#
# Figure S3
# 
# BiSSE / FISSE comparison across EEG sim datasets w MCMC for BiSSE significance 
# under full (6 param) BiSSE model
xx <- read.table("Feb2017_final_setsummaries.csv", stringsAsFactors=F, header=T, sep=",")

xxy <- xx[xx$True.SDD == "yes", ]
xxn <- xx[xx$True.SDD == "no", ]

#xxy <- xxy[order(xxy$bisse.sig, xxy$fisse.sig), ]
#xxn <- xxn[order(xxn$bisse.sig, xxn$fisse.sig), ]


quartz.options(height = 4, width=12)
plot.new()
par(oma=c(1,2,1,1))
mm <- matrix(c(1, 2,2), nrow=2, ncol=3, byrow=T)
layout(mm)

wid <- 0.25

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim=c(0, 16), ylim=c(0,1))


for (i in 1:nrow(xxy)){
	lines(c(i,i), y=c(xxy$bisse.r.sig[i], xxy$fisse.sig[i]), lwd=0.9, col="gray60")
	points(i, xxy$bisse.r.sig[i], pch=23, bg="blue", cex=1.8)
	#points(i, xxy$r.frac.sdd[i], pch=24, bg="blue", cex=1.8)
	points(i , xxy$fisse.sig[i], pch=21, bg="red", cex=1.6)	
	
	if (is.na(xxy$original_index_doubleblind[i])){
		mtext("*", side=1, at=i, line=1.1, cex=1.5)
	}
}

points(0.5, 0.9, pch=23, bg="blue", cex=1.8)
points(0.5, 0.8, pch=21, bg="red", cex=1.6)
text(0.7, 0.9, labels="BiSSE", cex=1.5, pos=4, font=3)
text(0.7, 0.8, labels="FiSSE", cex=1.5, pos=4, font=3)

axis(1, at=c(-2, seq(1, 15, by=1), 16), labels=NA)
axis(1, at=seq(1, 15, by=2), tick=F, cex.axis=1.2, padj=0.5)
mtext("Simulation scenario: true SDD", side=1, line=3.25, cex=1.2)

axis(2, at=seq(-0.1, 1.0, by=0.1), label=NA)
axis(2, at=seq(0, 1, by=0.2), las=1, tick=F, cex.axis=1.2)

mtext(side=2, text="Proportion significant", line= 3.7, cex=1.2)
mtext(side=2, text="A", line= 3.7, cex=1.4, las=1, at=1.05)

plot.new()
par(mar=c(5,5,1,1))
plot.window(xlim=c(0, 34.5), ylim=c(0,1))
polygon(x=c(-2,-2, 35,35), y=c(0,0.05, 0.05,0), border=F, col="gray80")

for (i in 1:nrow(xxn)){
	lines(c(i,i), y=c(xxn$bisse.r.sig[i], xxn$fisse.sig[i]), lwd=0.75, col="gray70")
	points(i, xxn$bisse.r.sig[i], pch=23, bg="blue", cex=1.8)
	#points(i, xxn$r.frac.sdd[i], pch=24, bg="blue", cex=1.8)
	points(i , xxn$fisse.sig[i], pch=21, bg="red", cex=1.6)	
	
	if (is.na(xxn$original_index_doubleblind[i])){
		mtext("*", side=1, at=i, line=1.1, cex=1.5)
	}
 
}

axis(1, at=c(-2, seq(1, 34, by=1), 36), labels=NA)
axis(1, at=seq(1, 33, by=2), labels= seq(17, 49, by=2), tick=F, cex.axis=1.2, padj=0.5)
mtext("Simulation scenario: no SDD", side=1, line=3.25, cex=1.2)
axis(2, at=seq(-0.1, 1.0, by=0.1), label=NA)
axis(2, at=seq(0, 1, by=0.2), las=1, tick=F, cex.axis=1.2)
#mtext(side=2, text="Type I error rate", line= 3.5, cex=1.2)
mtext(side=2, text="Proportion significant", line= 3.7, cex=1.2)
mtext(side=2, text="B", line= 3.7, cex=1.4, las=1, at=1.05)


 






