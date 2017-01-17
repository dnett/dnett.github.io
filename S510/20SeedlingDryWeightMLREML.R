d=read.delim("http://www.public.iastate.edu/~dnett/S510/SeedlingDryWeight2.txt")
d

plot(d[,2],d[,4]+rnorm(56,0,.2),
    xlab="Tray",ylab="Seedling Dry Weight",
    col=2*d[,1],pch="-",cex=2)
legend("topright",c("Genotype 1","Genotype 2"),fill=c(2,4),border=c(2,4))

d$Genotype=factor(d$Genotype)

library(nlme)

lme(SeedlingWeight~Genotype,random=~1|Tray,method="ML",data=d)

library(lme4)

lmer(SeedlingWeight~Genotype+(1|Tray),REML=F,data=d)

lme(SeedlingWeight~Genotype,random=~1|Tray,data=d)

lmer(SeedlingWeight~Genotype+(1|Tray),data=d)

