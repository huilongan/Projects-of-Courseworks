#bayesian analysis paradim#
bicy=c(16,9,10,13,19,20,18,17,35,55)
vehi=c(58,90,48,57,103,57,86,112,273,64)
y=bicy+vehi
#noninformative prior on (0,inf)X(0,inf)#
post.fun=function(alpha,beta,y){
  x=prod((1/beta(alpha,y))*(1/y)*(beta/(1+beta))^alpha*(1/(beta+1))^y)
  return(log(x))
}
post.fun(0.02,0.04,y)
miny=min(y);maxy=max(y)
alpha=seq(0.1,0.6,length.out=1200)
beta=seq(0.001,0.1,length.out=1200)
n=length(alpha)
m=length(beta)
mar.post=matrix(rep(NA),n,m)
for(i in 1:n){
  for(j in 1:m){
    mar.post[i,j]=post.fun(alpha[i],beta[j],y)
  }
}
#transform#
max(mar.post)
mode=max(mar.post)
mar.post=exp(mar.post-mode)
contour(alpha,beta,mar.post,ylim=c(0,0.01))
alpha.s=rep(NA,1000)
beta.s=rep(NA,1000)
for(i in 1:1000){
  a=sample(n,1,prob=apply(mar.post,1,sum));
  alpha.s[i]=alpha[a];
  beta.s[i]=sample(beta,1,prob=mar.post[a,]);
}
plot(beta.s~alpha.s,pch=16,col="red")
theta.s=matrix(rep(NA,1000,length(y)),1000,length(y))
num=length(y)
for(i in 1:1000){
  theta.s[i,]=rgamma(num,alpha.s[i]+y,beta.s[i]+1)
}
head(theta.s)
plot(y,apply(theta.s,2,median),xlab="observed",ylab="posterior estimate")
abline(0,1,lty=2)
for(i in 1:num){
  lines(x=c(y[i],y[i]),y=quantile(theta.s[,i],c(0.025,0.975)))
}