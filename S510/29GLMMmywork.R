setwd("c:/z/Courses/S510/Notes")

x=rep(c(0,5,10,15,20,25),each=10)
plant=factor(rep(1:30,each=2))

set.seed(31673)
u=rnorm(30,0,.7)
e=rnorm(60,0,.5)

#mistake here.  Should have used rep(u,each=2) in place of u.
y=rpois(60,exp(log(60)-.15*x+u+e))

write.table(data.frame(x=x,y=y),"26LeafInfectionData.txt",
            sep="\t",row.names=F,col.names=T,quote=F)

plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4)

#plot(x,y,xlab="Amount of Anti-Fungal Treatment",
#     ylab="Number of Infected Cells",col=4,ylim=c(0,40))




#pdf("26PoissonFig1.pdf")
plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)
#dev.off()



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


#pdf("26PoissonFig2.pdf")
plot(x,y,xlab="Amount of Anti-Fungal Treatment",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)
xgrid=seq(0,25,by=.1)
lines(xgrid,exp(b[1]+b[2]*xgrid),col=2,lwd=1.5)
#dev.off()

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

#pdf("26PoissonFig3.pdf")
plot(x,y,xlab="Amount of Anti-Fungal Treatment",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5,
     xlim=c(14,21),ylim=c(0,18))
xgrid=seq(0,25,by=.1)
lines(xgrid,exp(b[1]+b[2]*xgrid),col=2,lwd=1.5)
abline(h=5,lty=2)
lines(c(h,h),c(-1,5),lwd=1.5)
lines(c(ci[1],ci[1]),c(-1,5),lwd=1.5,col="purple")
lines(c(ci[2],ci[2]),c(-1,5),lwd=1.5,col="purple")
#dev.off()

d=resid(o,type="deviance")

#pdf("26PoissonFig4.pdf")
plot(fitted(o),d,
     xlab="Estimated Mean",
     ylab="Deviance Residual",
     cex.lab=1.4,
     pch=16,col=4)
abline(h=0,lty=2)
#dev.off()

r=resid(o,type="pearson")

#pdf("26PoissonFig5.pdf")
plot(fitted(o),r,
     xlab="Estimated Mean",
     ylab="Pearson Residual",
     cex.lab=1.4,
     pch=16,col=4)
abline(h=0,lty=2)
#dev.off()

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




library(lme4)
leaf=factor(1:60)

o=glmer(y~x+(1|leaf),family=poisson(link = "log"))
summary(o)
b=fixef(o)
v=vcov(o)
b
v

sigma.sq.leaf=unlist(VarCorr(o))
sigma.sq.leaf

pdf("27fig1.pdf")
plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)
lines(xgrid,exp(b[1]+b[2]*xgrid),col=2)
lines(xgrid,exp(b[1]+b[2]*xgrid+sigma.sq.leaf/2),col=3)
legend(10,200,c("Marginal Mean","Conditional Mean"),lty=1,col=c(3,2))
dev.off()



d=data.frame(plant,leaf,x,y)
head(d)
tail(d)

o=glmer(y~x+(1|plant)+(1|leaf),family=poisson(link = "log"))
summary(o)
b=fixef(o)
v=vcov(o)
vc=unlist(VarCorr(o))
b
v
vc
lines(xgrid,exp(b[1]+b[2]*xgrid),col=4)


o=glmer(y~x+(1|plant)+(1|leaf),family=poisson(link = "log"))
summary(o)


uhat=ranef(o)
uplant=unlist(uhat$plant)
uleaf=unlist(uhat$leaf)

pdf("27fig2.pdf")
plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)

lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[3]+uleaf[5]),col=2)
lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[3]+uleaf[6]),col=2)
lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[30]+uleaf[59]),col=3)
lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[30]+uleaf[60]),col=3)
dev.off()

d=read.delim("http://www.public.iastate.edu/~dnett/S511/Trout.txt")
d

pdf("27fig3.pdf")
plot(d$dose,d$tumor/d$total,col=4,pch=19,
     xlab="Dose",
     ylab="Proportion of Fish with Tumor")
dev.off()

d$dosef=gl(5,4)
tank=factor(1:20)
o=glmer(cbind(tumor,total-tumor)~dosef+(1|tank),
        family=binomial(link="logit"),nAGQ=20,
        data=d)
summary(o)

b=fixef(o)
v=vcov(o)
vc=unlist(VarCorr(o))
b
round(v,3)
vc

#Estimated tumor probability for fish in a
#"typical" tank (tank effect = 0)
#treated with 0.10 units (dose 4) is

1/(1+exp(-(b[1]+b[4])))

#Estimated distribution of tumor probabilities
#for tanks treated with 0.10 units (dose 4):

set.seed(5369)
sim.tank.effects=rnorm(100000,0,sqrt(vc))
sim.tumor.probs=1/(1+exp(-(b[1]+b[4]+sim.tank.effects)))
pdf("27fig4.pdf")
hist(sim.tumor.probs,col="gray",probability=T,nclass=50,
     ylab="Density",xlab="Tumor Probability",
     main="Estimated Distribution for Dose=0.10")
box()
abline(v=1/(1+exp(-(b[1]+b[4]))),col=4,lwd=2)
abline(v=mean(sim.tumor.probs),col=2,lwd=2)
dev.off()


#How would the picture change if the
#tank standard deviation had been estimated
#to be 1.0 instead of 0.0979?

set.seed(5369)
sim.tank.effects=rnorm(100000,0,1)
sim.tumor.probs=1/(1+exp(-(b[1]+b[4]+sim.tank.effects)))
pdf("27fig5.pdf")
hist(sim.tumor.probs,col="gray",probability=T,nclass=50,
     ylab="Density",xlab="Tumor Probability",
     main="Estimated Distribution for Dose=0.10")
box()
abline(v=1/(1+exp(-(b[1]+b[4]))),col='blue',lwd=2)
abline(v=mean(sim.tumor.probs),col='red',lwd=2)
dev.off()

#Testing dose 4 vs. 5
cc=c(0,0,0,1,-1)
est=drop(t(cc)%*%b)
est
se=drop(sqrt(t(cc)%*%v%*%cc))
z.stat=est/se
z.stat
p.value=2*(1-pnorm(abs(z.stat),0,1))
p.value 

#Confidence interval
est+c(-2,2)*se

exp(-est)

exp(-est+c(-2,2)*se)

