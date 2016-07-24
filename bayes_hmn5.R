set.seed(1000)
theta.s=rnorm(5000,5.1,0.1)
rep.s=matrix(rep(NA,5000*1000),5000,1000)
test.stat=rep(NA,5000)
for (i in 1:5000){
  rep.s[i,]=rnorm(1000,theta.s[i],1)
}
test.stat=apply(rep.s,1,function(x) max(abs(x)))
hist(test.stat)
abline(v=8.1,col="red")
p.val=length(which(test.stat>8.1))/5000
p.val

#part b
rm(list=ls())
set.seed(1000)
theta.s=runif(5000,-10^5,10^5)
rep.s=matrix(rep(NA,5000*1000),5000,1000)
test.stat=rep(NA,5000)
for (i in 1:5000){
  rep.s[i,]=rnorm(1000,theta.s[i],1)
}
test.stat=apply(rep.s,1,function(x) max(abs(x)))
hist(test.stat)
p.val=length(which(test.stat>8.1))/5000
p.val
