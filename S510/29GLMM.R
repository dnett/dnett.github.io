d=read.delim("http://dnett.github.io/S510/LeafInfectionData.txt")
d
x=d$x
y=d$y

plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)


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

xgrid=seq(0,25,by=.01)
plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)
lines(xgrid,exp(b[1]+b[2]*xgrid),col=2)
lines(xgrid,exp(b[1]+b[2]*xgrid+sigma.sq.leaf/2),col=3)
legend(10,200,c("Marginal Mean","Conditional Mean"),lty=1,col=c(3,2))

plant=gl(30,2)
plant
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

plot(x,y,xlab="Amount of Anti-Fungal Chemical",
     ylab="Number of Infected Cells",col=4,cex.lab=1.5)

lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[3]+uleaf[5]),col=2)
lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[3]+uleaf[6]),col=2)
lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[30]+uleaf[59]),col=3)
lines(xgrid,exp(b[1]+b[2]*xgrid+uplant[30]+uleaf[60]),col=3)


d=read.delim("http://www.public.iastate.edu/~dnett/S511/Trout.txt")
d

plot(d$dose,d$tumor/d$total,col=4,pch=19,
     xlab="Dose",
     ylab="Proportion of Fish with Tumor")

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

hist(sim.tumor.probs,col="gray",probability=T,nclass=50,
     ylab="Density",xlab="Tumor Probability",
     main="Estimated Distribution for Dose=0.10")
box()
abline(v=1/(1+exp(-(b[1]+b[4]))),col=4,lwd=2)
abline(v=mean(sim.tumor.probs),col=2,lwd=2)

#How would the picture change if the
#tank standard deviation had been estimated
#to be 1.0 instead of 0.0979?

set.seed(5369)
sim.tank.effects=rnorm(100000,0,1)
sim.tumor.probs=1/(1+exp(-(b[1]+b[4]+sim.tank.effects)))
hist(sim.tumor.probs,col="gray",probability=T,nclass=50,
     ylab="Density",xlab="Tumor Probability",
     main="Estimated Distribution for Dose=0.10")
box()
abline(v=1/(1+exp(-(b[1]+b[4]))),col='blue',lwd=2)
abline(v=mean(sim.tumor.probs),col='red',lwd=2)

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

