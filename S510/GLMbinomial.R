d=read.delim("http://dnett.github.io/S510/Disease.txt")
head(d)

d$ses=as.factor(d$ses)
d$sector=as.factor(d$sector)

o=glm(disease~age+ses+sector,
      family=binomial(link=logit),
      data=d)

summary(o)

coef(o)
round(vcov(o),3)
confint(o)

oreduced=glm(disease~age+sector,
      family=binomial(link=logit),
      data=d)

anova(oreduced,o,test="Chisq")

o=oreduced

anova(o,test="Chisq")

head(model.matrix(o))

b=coef(o)
b

ci=confint(o)
ci

#How should we interpret our estimate of
#the slope coefficient on age?

exp(b[2])

#All else equal, the odds of disease are
#about 1.027 times greater for someone age
#x+1 than for someone age x. An increase of
#one year in age is associated with an
#increase in the odds of disease by about 2.7%.
#A 95% confidence interval for the multiplicative
#increase factor is

exp(ci[2,])

#How should we interpret our estimate of
#the slope coefficient on sector?

exp(b[3])

#All else equal, the odds of disease are
#about 3.26 times greater for someone
#living in sector 2 than for someone living
#in sector one.

#A 95% confidence interval for the multiplicative
#increase factor is

exp(ci[3,])

#Estimate the probability that a randomly 
#selected 40-year-old living in sector 2
#has the disease.

x=c(1,40,1)

1/(1+exp(-t(x)%*%b))

#Approximate 95% confidence interval
#for the probability in question.

sexb=sqrt(t(x)%*%vcov(o)%*%x)
cixb=c(t(x)%*%b-2*sexb,t(x)%*%b+2*sexb)
1/(1+exp(-cixb))

#Plot estimated probabilities as a function
#of age for each sector.

x=1:85
plot(x,1/(1+exp(-(b[1]+b[2]*x))),ylim=c(0,1),
     type="l",col=4,lwd=2,xlab="Age",
     ylab="Estimated Probability of Disease")
lines(x,1/(1+exp(-(b[1]+b[2]*x+b[3]))),col=2,lwd=2)
legend("bottomright", legend=c("Sector 1","Sector 2"), col=c(4,2),lwd=2)

#Now let's consider an example with binomial
#rather than Bernoulli responses.

d=read.delim("http://www.public.iastate.edu/~dnett/S511/Trout.txt")
d

#Let's plot observed tumor proportions
#for each tank.

plot(d$dose,d$tumor/d$total,col=4,pch=19,
     xlab="Dose",
     ylab="Proportion of Fish with Tumor")

#Let's fit a logistic regression model
#dose is a quantitative explanatory variable.

o=glm(cbind(tumor,total-tumor)~dose,
      family=binomial(link=logit),
      data=d)

summary(o)

#Let's plot the fitted curve.

b=coef(o)
u=seq(0,.25,by=0.001)
xb=b[1]+u*b[2]
pihat=1/(1+exp(-xb))
lines(u,pihat,col=2,lwd=1.3)

#Let's use a reduced versus full model
#likelihood ratio test to test for
#lack of fit relative to the
#saturated model.

1-pchisq(deviance(o),df.residual(o))

#We could try adding higher-order
#polynomial terms, but let's just
#skip right to the model with dose
#as a categorical variable.

d$dosef=gl(5,4)
d

o=glm(cbind(tumor,total-tumor)~dosef,
      family=binomial(link=logit),
      data=d)

summary(o)

#Let's add the new fitted values to our plot.

fitted(o)

points(d$dose,fitted(o),pch="_",cex=3,col=3)

#The fit looks good, but let's formally
#test for lack of fit.

1-pchisq(deviance(o),df.residual(o))

#There is still a significant lack of fit
#when comparing to the saturated model.

#The problem is over dispersion, otherwise
#known in this case as extra binomial variation. 


#Let's estimate the dispersion parameter.

phihat=deviance(o)/df.residual(o)
phihat

#We can obtain the same estimate by using
#the deviance residuals.

di=residuals(o,type="deviance")
sum(di^2)/df.residual(o)

#We can obtain an alternative estimate by
#using the Pearson residuals.

ri=residuals(o,type="pearson")
phihat=sum(ri^2)/df.residual(o)
phihat

#Now we will conduct a quasilikelihood analysis
#that accounts for overdispersion.

oq=glm(cbind(tumor,total-tumor)~dosef,
      family=quasibinomial(link=logit),
      data=d)

summary(oq)

#Test for the effect of dose on the response.

drop1(oq,test="F")

#There is strong evidence that
#the probability of tumor formation
#is different for different doses
#of the toxicant.

#Let's test for a difference between
#the top two doses.

b=coef(oq)
b
v=vcov(oq)
v
se=sqrt(t(c(0,0,0,-1,1))%*%v%*%c(0,0,0,-1,1))

tstat=(b[5]-b[4])/se
pval=2*(1-pt(abs(tstat),df.residual(oq)))
pval

