#An example from "Design of Experiments: Statistical
#Principles of Research Design and Analysis"
#2nd Edition by Robert O. Kuehl

d=read.delim("http://dnett.github.io/S510/PlantDensity.txt")

d

names(d)=c("x","y")
head(d)

plot(d[,1],d[,2],col=4,pch=16,xlab="Plant Density",
     ylab="Grain Yield")


y=d$y
x=(d$x-mean(d$x))/10
x

n=nrow(d)

x1=matrix(1,nrow=n,ncol=1)
x2=cbind(x1,x)
x3=cbind(x2,x^2)
x4=cbind(x3,x^3)
x5=matrix(model.matrix(~0+factor(x)),nrow=n)
I=diag(rep(1,n))

library(MASS)
proj=function(x){
  x%*%ginv(t(x)%*%x)%*%t(x)
}

p1=proj(x1)
p2=proj(x2)
p3=proj(x3)
p4=proj(x4)
p5=proj(x5)

t(y)%*%(p2-p1)%*%y
t(y)%*%(p3-p2)%*%y
t(y)%*%(p4-p3)%*%y
t(y)%*%(p5-p4)%*%y
t(y)%*%(I-p5)%*%y
t(y)%*%(I-p1)%*%y

o=lm(y~x+I(x^2)+I(x^3)+I(x^4),data=d)
anova(o)


X=x5
(p2-p1)%*%X
5*((p2-p1)%*%X)[15,]

#Let's add the best fitting simple linear regression
#line to our plot.

o=lm(y~x,data=d)

u=seq(0,60,by=.01) #overkill here but used later.

lines(u,coef(o)[1]+coef(o)[2]*u,col=2)

#The fit doesn't look very good.
#Let's formally test for lack of fit.

o=lm(y~x+factor(x),data=d)
anova(o)

#It looks like a linear fit is inadequate.
#Let's try a quadratic fit.

o=lm(y~x+I(x^2)+factor(x),data=d)
anova(o)

#It looks like a quadratic fit is adequate.
#Let's estimate the coefficients for the best
#quadratic fit.

b=coef(lm(y~x+I(x^2),data=d))

#Let's add the best fitting quadratic curve
#to our plot.

lines(u,b[1]+b[2]*u+b[3]*u^2,col=3)

#Let's add the treatment group means to our plot.

trt.means=tapply(d$y,d$x,mean)

points(unique(d$x),trt.means,pch="X")

#The quartic fit will pass through the treatment
#means.

b=coef(lm(y~x+I(x^2)+I(x^3)+I(x^4),data=d))
lines(u,b[1]+b[2]*u+b[3]*u^2+b[4]*u^3+b[5]*u^4,col=1)
