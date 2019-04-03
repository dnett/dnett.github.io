d=read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d

plot(d[,2],d[,4]+rnorm(56,0,.2),
    xlab="Tray",ylab="Seedling Dry Weight",
    col=2*d[,1],pch="-",cex=2)
legend("topright",c("Genotype 1","Genotype 2"),fill=c(2,4),border=c(2,4))

d$Genotype=factor(d$Genotype)

library(lme4)

#Fit the linear mixed-effects model
#with fixed genotype effects and
#random tray effects.
o = lmer(SeedlingWeight~Genotype+(1|Tray),data=d)

#uhat is the vector of the EBLUPs of Tray effects.
ranef(o)
uhat = ranef(o)$Tray[[1]]
uhat

#Get EBLUPs of genotype mean + tray effects

betahat = fixef(o)
betahat
estGeno1Mean = as.numeric(betahat[1])
estGeno2Mean = as.numeric(betahat[1] + betahat[2])
EBLUPs = c(estGeno1Mean + uhat[1:4],
           estGeno2Mean + uhat[5:8])

#Compare EBLUPs with tray averages.
trayAverages = tapply(d$SeedlingWeight, d$Tray, FUN = mean)
rbind(trayAverages, EBLUPs)
estGeno1Mean
estGeno2Mean

