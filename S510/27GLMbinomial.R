#Now let's consider an example with binomial
#rather than Bernoulli responses.

d=read.delim("http://dnett.github.io/S510/Trout.txt")
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

