d=read.delim("http://www.public.iastate.edu/~dnett/S510/Disease.txt")
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
