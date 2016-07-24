#bayesian analysis paradim#
bicy=c(16,9,10,13,19,20,18,17,35,55)
vehi=c(58,90,48,57,103,57,86,112,273,64)
y=bicy+vehi
#set the simulation sequence#
log.adivdb=seq(3,7,0.0025)
log.b=seq(-8,-1,0.005)
#log-marginal function
#notice built-in gamma fun allows only a samll range
#alpha=exp(log.adivdb+log.b)
#beta=exp(log.b)
alpha=matrix(rep(NA,length(log.adivdb)*length(log.b)),length(log.adivdb),length(log.b))
beta=alpha
for(i in 1:length(log.adivdb)){
  for(j in 1:length(log.b)){
    beta[i,j]=exp(log.b[j])
    alpha[i,j]=exp(log.adivdb[i]+log.b[j])
  }
}
mar.pos.fun=function(logapb,logb,f,y){
  b=exp(logb)
  a=exp(logapb+logb)
  bi=rep(NA,length(y))
  k=1
  for (i in y){
    bi[k]=sum(log(seq(a,i+a-1,1)))
    k =k+1
  }
  return(log(a)+log(b)+f*(log(a)+log(b))-f*(a+b)+10*a*log(b)+sum(bi)
         )
}
#log-posterior density
log.mar.poster=matrix(rep(NA,length(log.adivdb)*length(log.b)),length(log.adivdb),length(log.b))
for(i in 1:length(log.adivdb)){
  for(j in 1:length(log.b)){
    log.mar.poster[i,j]=mar.pos.fun(log.adivdb[i],log.b[j],0.001,y)
  }
}
#calculate the posterior density
mar.poster=exp(log.mar.poster-max(log.mar.poster))
contour(log.adivdb,log.b,mar.poster,xlab="log(alpha/beta)",ylab="log(beta)",drawlabels = F)
#contour plot
mid.poster=log.mar.poster/max(log.mar.poster)
contour(log.adivdb,log.b,mid.poster,xlab="log(alpha/beta)",ylab="log(beta)",drawlabels = F)
alpha.s=rep(NA,1000)
beta.s=alpha.s
theta.s=matrix(rep(NA,1000*10),1000,10)
set.seed(1087)
for(i in 1:1000){
  a=sample(length(log.adivdb),1,prob=apply(mid.poster,1,sum))
  b=sample(length(log.b),1,prob=mid.poster[a,])
  alpha.s[i]=alpha[a,b]
  beta.s[i]=beta[a,b]
  theta.s[i,]=rgamma(10,alpha.s[i]+y,beta.s[i]+1)
}
plot(log(alpha.s/beta.s),log(beta.s))
plot(y,apply(theta.s,2,median),xlab="observed",ylab="posterior estimate",ylim=c(0,500),xlim=c(0,500))
abline(0,1,lty=2)
for(i in 1:length(y)){
  lines(x=c(y[i],y[i]),y=quantile(theta.s[,i],c(0.025,0.975)))
}