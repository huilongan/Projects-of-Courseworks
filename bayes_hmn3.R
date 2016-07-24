set.seed(1082)
theta.s=rbeta(2000,8,14)
rep.s=matrix(rep(NA,2000*100),2000,100)
for (i in 1:2000){
  k=1
  zero.n=0
  while (zero.n<=13) {
    rep.s[i,k]=rbinom(1,1,theta.s[i])
    if(rep.s[i,k]==0){zero.n=zero.n+1}
    k=k+1
    if(zero.n==13){
      break;
    }
  }
}
count=rep(NA,2000)
for (i in 1:2000){
  t=as.vector(na.omit(rep.s[i,]))
  len=length(t)
  switch.n=0
  for (j in 2:len){
    if(t[j]!=t[j-1]){
     switch.n=switch.n+1 
    }
  }
  count[i]=switch.n
}
hist(count,breaks=20)
abline(v=3,col="red")
test.stat=length(which(count<=3))/2000
test.stat
##
