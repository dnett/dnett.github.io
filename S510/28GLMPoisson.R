d=read.delim("http://dnett.github.io/S510/LeafInfectionData.txt")
d
x=d$x
y=d$y

plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)


o=glm(y~x,family=poisson(link = "log"))
summary(o)

#MLE of beta vector
b=coef(o)
b

#Estimated variance of the MLE
v=vcov(o)
v

#Test Statistic
z=b[2]/sqrt(v[2,2])
z

#p-value
2*pnorm(z)

#Likelihood Ratio Test
anova(o,test="Chisq")

plot(x,y,xlab="Amount of Anti-Fungal Treatment",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)
xgrid=seq(0,25,by=.1)
lines(xgrid,exp(b[1]+b[2]*xgrid),col=2,lwd=1.5)

exp(b[2])

c(exp(b[2]-2*sqrt(v[2,2])),exp(b[2]+2*sqrt(v[2,2])))

cc=c(1,15)
se=sqrt(t(cc)%*%v%*%cc)
exp(t(cc)%*%b-2*se)
exp(t(cc)%*%b+2*se)


h=(log(5)-b[1])/b[2]
h

Dhat=c(-1/b[2],(b[1]-log(5))/b[2]^2)

seh=sqrt(t(Dhat)%*%v%*%Dhat)

ci=c(h-2*seh,h+2*seh)
ci

plot(x,y,xlab="Amount of Anti-Fungal Treatment",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5,
     xlim=c(14,21),ylim=c(0,18))
xgrid=seq(0,25,by=.1)
lines(xgrid,exp(b[1]+b[2]*xgrid),col=2,lwd=1.5)
abline(h=5,lty=2)
lines(c(h,h),c(-1,5),lwd=1.5)
lines(c(ci[1],ci[1]),c(-1,5),lwd=1.5,col="purple")
lines(c(ci[2],ci[2]),c(-1,5),lwd=1.5,col="purple")

d=resid(o,type="deviance")

plot(fitted(o),d,
     xlab="Estimated Mean",
     ylab="Deviance Residual",
     cex.lab=1.4,
     pch=16,col=4)
abline(h=0,lty=2)

r=resid(o,type="pearson")

plot(fitted(o),r,
     xlab="Estimated Mean",
     ylab="Pearson Residual",
     cex.lab=1.4,
     pch=16,col=4)
abline(h=0,lty=2)

deviance(o)
sum(d^2)
sum(r^2)
1-pchisq(deviance(o),60-2)
1-pchisq(sum(r^2),60-2)

#Estimates of the dispersion parameter

deviance(o)/df.residual(o)

sum(r^2)/df.residual(o)

oq=glm(y~x,family=quasipoisson(link = "log"))
summary(oq)

#MLE of beta vector
b=coef(oq)
b

#Estimated variance of the MLE
v=vcov(oq)
v

#Test Statistic
tstat=b[2]/sqrt(v[2,2])
tstat

#p-value
2*pt(tstat,60-2)

#Likelihood Ratio Test
anova(oq,test="F")
