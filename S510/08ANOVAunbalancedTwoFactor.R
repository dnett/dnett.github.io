time=factor(rep(c(3,6),each=5))
temp=factor(rep(c(20,30,20,30),c(2,3,4,1)))
a=time
b=temp
y=c(3,5,11,13,15,5,6,6,7,16)
d=data.frame(time,temp,y)
d


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
I=diag(rep(1,10))

SumOfSquares=c(
t(y)%*%(p2-p1)%*%y,
t(y)%*%(p3-p2)%*%y,
t(y)%*%(p4-p3)%*%y,
t(y)%*%(I-p4)%*%y,
t(y)%*%(I-p1)%*%y)

Source=c(
"Time|1",
"Temp|1,Time",
"Time x Temp|1,Time,Temp",
"Error",
"C. Total")

data.frame(Source,SumOfSquares)

o=lm(y~time+temp+time:temp,data=d)

anova(o)

x=x4
fractions((p2-p1)%*%x)
fractions(2*(p2-p1)%*%x)[1,]

fractions((p3-p2)%*%x)
fractions((25/15)*(p3-p2)%*%x)[1,]

fractions((p4-p3)%*%x)
fractions((25/6)*(p4-p3)%*%x)[1,]

x1=matrix(1,nrow=nrow(d),ncol=1)
x1
x2=cbind(x1,model.matrix(~0+b))
x2
x3=cbind(x2,model.matrix(~0+a))
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
I=diag(rep(1,10))

SumOfSquares=c(
t(y)%*%(p2-p1)%*%y,
t(y)%*%(p3-p2)%*%y,
t(y)%*%(p4-p3)%*%y,
t(y)%*%(I-p4)%*%y,
t(y)%*%(I-p1)%*%y)

Source=c(
"Temp|1",
"Time|1,Temp",
"Temp x Time|1,Temp,Time",
"Error",
"C. Total")

data.frame(Source,SumOfSquares)

o=lm(y~temp+time+temp:time,data=d)

anova(o)

x=x4
fractions((p2-p1)%*%x)
fractions((30/12)*(p2-p1)%*%x)[1,]


fractions((p3-p2)%*%x)
fractions((3/2)*(p3-p2)%*%x)[1,]

fractions((p4-p3)%*%x)
fractions((25/6)*(p4-p3)%*%x)[1,]


test=function(lmout,C,d=0){
  b=coef(lmout)
  V=vcov(lmout)
  dfn=nrow(C)
  dfd=lmout$df
  Cb.d=C%*%b-d
  Fstat=drop(t(Cb.d)%*%solve(C%*%V%*%t(C))%*%Cb.d/dfn)
  pvalue=1-pf(Fstat,dfn,dfd)
  list(Fstat=Fstat,pvalue=pvalue)
}

o=lm(y~0+temp:time)

#Test for time main effect

C=matrix(c(
.5,.5,-.5,-.5
),nrow=1,byrow=T)

test(o,C)

#ANOVA Test for time|1

C=matrix(c(
2/5,3/5,-4/5,-1/5 
),nrow=1,byrow=T)

test(o,C)


#ANOVA Test for time|1,temp

C=matrix(c(
16/25,9/25,-16/25,-9/25 
),nrow=1,byrow=T)

test(o,C)

#Test for temp main effect

C=matrix(c(
.5,-.5,.5,-.5
),nrow=1,byrow=T)

test(o,C)

#ANOVA Test for temp|1

C=matrix(c(
1/3,-3/4,2/3,-1/4
),nrow=1,byrow=T)

test(o,C)

#ANOVA Test for temp|1,time

C=matrix(c(
3/5,-3/5,2/5,-2/5
),nrow=1,byrow=T)

test(o,C)

#Test for interactions

C=matrix(c(
1,-1,-1,1
),nrow=1,byrow=T)

test(o,C)

#Type III Sum of Squares for 
#SS(time|1,temp,time x temp)

x0=x[,1:3]
x0[10,]=c(1,1,-1)
x0

anova(lm(y~0+x0),lm(y~0+x))