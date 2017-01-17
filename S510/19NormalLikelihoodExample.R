set.seed(5912321)

set.seed(59123800)
x=round(rnorm(5,7,2),1)
x
xx=seq(4,9,by=0.01)

plot(xx,dnorm(xx,5,sqrt(.5)),axes=F,xlim=c(4,9),pch=" ",xlab=" ",ylab="Density",
     cex.lab=1.4,ylim=c(0,dnorm(5,5,sqrt(.5))+.01),xaxs="i",yaxs="i")
axis(1,labels=x,at=x)
box()

mu=5
ss=.5
lines(xx,dnorm(xx,mu,sqrt(ss)),col=4,lwd=2)
for(i in 1:5){
  lines(c(x[i],x[i]),c(0,dnorm(x[i],mu,sqrt(ss))),col=2,lwd=2)
}
prod(dnorm(x,mu,sqrt(ss)))


#######################################################
plot(xx,dnorm(xx,5,sqrt(.5)),axes=F,xlim=c(4,9),pch=" ",xlab=" ",ylab="Density",
     cex.lab=1.4,ylim=c(0,0.01+dnorm(5,5,sqrt(.5))),xaxs="i",yaxs="i")
axis(1,labels=x,at=x)
box()
mu=7
ss=4
lines(xx,dnorm(xx,mu,sqrt(ss)),col=4,lwd=2)
for(i in 1:5){
  lines(c(x[i],x[i]),c(0,dnorm(x[i],mu,sqrt(ss))),col=2,lwd=2)
}

prod(dnorm(x,mu,sqrt(ss)))

##############################################
plot(xx,dnorm(xx,5,sqrt(.5)),axes=F,xlim=c(4,9),pch=" ",xlab=" ",ylab="Density",
     cex.lab=1.4,ylim=c(0,0.01+dnorm(5,5,sqrt(.5))),xaxs="i",yaxs="i")
axis(1,labels=x,at=x)
box()

mu=mean(x)
ss=(4/5)*sd(x)^2
lines(xx,dnorm(xx,mu,sqrt(ss)),col=4,lwd=2)
for(i in 1:5){
  lines(c(x[i],x[i]),c(0,dnorm(x[i],mu,sqrt(ss))),col=2,lwd=2)
}
prod(dnorm(x,mu,sqrt(ss)))
mu
ss
