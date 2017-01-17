d=read.delim("http://www.public.iastate.edu/~dnett/S510/dietdrug.txt")
d

d$diet=factor(d$diet)
d$drug=factor(d$drug)

a=d$diet
b=d$drug
y=d$weightgain

x1=matrix(1,nrow=nrow(d),ncol=1)
x1
x2=cbind(x1,model.matrix(~0+a))
x2
x3=cbind(x2,model.matrix(~0+b))
x3
x4=model.matrix(~0+b:a)
x4

library(MASS)
proj=function(x){
  x%*%ginv(t(x)%*%x)%*%t(x)
}

p1=proj(x1)
p2=proj(x2)
p3=proj(x3)
p4=proj(x4)
I=diag(rep(1,12))

SumOfSquares=c(
t(y)%*%(p2-p1)%*%y,
t(y)%*%(p3-p2)%*%y,
t(y)%*%(p4-p3)%*%y,
t(y)%*%(I-p4)%*%y,
t(y)%*%(I-p1)%*%y)

Source=c(
"Diet|1",
"Drug|1,Diet",
"Diet x Drug|1,Diet,Drug",
"Error",
"C. Total")

data.frame(Source,SumOfSquares)


o=lm(weightgain~diet+drug+diet:drug,data=d)

anova(o)

x=x4
fractions((p2-p1)%*%x)
fractions((p3-p2)%*%x)

p3p2x=(p3-p2)%*%x

fractions(p3p2x[1,]-p3p2x[3,])

fractions(p3p2x[1,]-p3p2x[5,])

fractions((p4-p3)%*%x)

p4p3x=(p4-p3)%*%x

fractions(2*(p4p3x[1,]-p4p3x[3,]))

fractions(2*(p4p3x[1,]-p4p3x[5,]))

