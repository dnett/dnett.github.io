cr=factor(rep(rep(1:2,each=4),2))
cr
fr=factor(rep(rep(1:2,each=2),4))
fr

baker=factor(rep(1:4,each=4))
baker

judge=factor(c(1,2,1,3,2,4,3,4,5,6,5,7,6,8,7,8))

cbind(cr,fr,baker,judge)

X=model.matrix(~cr+fr+cr:fr)
beta=c(7,-4,-2,3)

Z=cbind(model.matrix(~0+baker),
        model.matrix(~0+baker:fr),
        model.matrix(~0+judge)
)

set.seed(99120)

bakerrf=rnorm(4,0,1)
judgerf=rnorm(8,0,2)
halfcakerf=rnorm(8,0,.5)
u=c(bakerrf,halfcakerf,judgerf)
e=rnorm(16,0,.5)

y=X%*%beta+Z%*%u+e

y=round(y,1)

y

d=data.frame(cr=cr,fr=fr,baker=baker,judge=judge,y=y)
d
