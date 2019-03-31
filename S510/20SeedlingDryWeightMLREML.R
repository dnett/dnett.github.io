d=read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d

plot(d[,2],d[,4]+rnorm(56,0,.2),
    xlab="Tray",ylab="Seedling Dry Weight",
    col=2*d[,1],pch="-",cex=2)
legend("topright",c("Genotype 1","Genotype 2"),fill=c(2,4),border=c(2,4))

d$Genotype=factor(d$Genotype)

library(lme4)

#Maximum Likelihood Estimation
lmer(SeedlingWeight~Genotype+(1|Tray),REML=F,data=d)

#REML Estimation
lmer(SeedlingWeight~Genotype+(1|Tray),data=d)

ranef(lmer(SeedlingWeight~Genotype+(1|Tray),data=d))

#Obtain EBLUPs of Tray Effects (see slide set 21)
ranef(lmer(SeedlingWeight~Genotype+(1|Tray),data=d))
