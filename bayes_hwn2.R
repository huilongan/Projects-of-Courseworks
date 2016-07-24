getwd()
setwd("c:/users/an/desktop")
data=read.table("data515.txt",header=T)
names(data)
logodd.s.1=log((data$treated.deaths)/(data$treated.total-data$treated.deaths))
logodd.s.2=log((data$control.deaths)/(data$control.total-data$control.deaths))
logodd.s=logodd.s.1-logodd.s.2
sigma.s.1=1/data$treated.deaths+1/(data$treated.total-data$treated.deaths)
sigma.s.2=1/data$control.deaths+1/(data$control.total-data$control.deaths)
sigma.s=sigma.s.1+sigma.s.2
#posterior density for tau#
tau.pos.fun=function(tau,sigma,y){
  mu=sum(y/(sigma+tau^2))/sum(1/(sigma+tau^2))
  return(
    (sum(1/(sigma+tau^2)))^(-0.5)*
    prod((sigma+tau^2)^(-0.5))*
    exp(-0.5*sum((y-mu)^2/(sigma+tau^2)))
  )
}
mu.s=mean(logodd.s)
tau=seq(0,0.6,0.01)
tau.pos=rep(NA,length(tau))
for (i in 1:length(tau)){
tau.pos[i]=tau.pos.fun(tau[i],sigma.s,logodd.s)}
tau.pos.norm=tau.pos/sum(tau.pos*0.01)
plot(tau.pos.norm~tau,type='l',main="posterior density function",ylab="density")
#culmulated density#
cdf.pos.tau=cumsum(tau.pos*0.1)
plot(cdf.pos.tau~tau,type="l")
#simulate expectation#
set.seed(1023)
expect.fun=function(tau,j,sigma,y){
  mu=sum(y/(sigma+tau^2))/sum(1/(sigma+tau^2))##mu hat
  varian=(sum(1/(sigma+tau^2)))^(-1)
  mu.s=rnorm(1000,mu,sqrt(varian))
  mean.theta=((1/sigma[j])*y[j]+(1/tau^2)*mu.s)/((1/sigma[j])^2+(1/tau)^2)
  var.theta=1/((1/sigma[j])^2+(1/tau)^2)
  theta.s=rnorm(1000,mean=mean.theta,sd=sqrt(var.theta))
  return(mean(theta.s))
}
#test: expect.fun(0.02,3,sigma.s,logodd.s)
expect.mat=matrix(rep(NA,length(logodd.s)*length(tau)),length(logodd.s),length(tau))
expect.f.mat=expect.mat
for(j in 1:length(logodd.s)){
  for(i in 1:length(tau)){
    if(tau[i]==0){expect.mat[j,i]=sum(logodd.s/sigma.s)/sum(1/sigma.s)}
    else{
      mu.hat=sum(logodd.s/(sigma.s+tau[i]^2))/sum(1/(sigma.s+tau[i]^2))
      expect.mat[j,i]=expect.fun(tau[i],j,sigma.s,logodd.s)}
      expect.f.mat[j,i]=(1/(1/sigma.s[j]+1/tau[i]^2))*(logodd.s[j]/sigma.s[j]+mu.hat/tau[i]^2)
    
  }
} 
plot(1,ylim=c(-0.6,0.3),xlim=c(0,0.6),main="expect value of theta given tau and y",xlab="tau",ylab="expect")
for(i in 1:length(logodd.s)){
lines(tau,expect.mat[i,])
}
text(locator(),labels=seq(1:length(logodd.s)))
#formula plot
for(i in 1:length(logodd.s)){
  lines(tau,expect.f.mat[i,])
  text(tau[length(tau)],expect.f.mat[i,length(expect.f.mat[i,])],labels=i)
}
#sd-plot
sd.mat=matrix(rep(NA,length(logodd.s)*length(tau)),length(logodd.s),length(tau))
for (j in 1:length(logodd.s)){
  for (i in 1:length(tau)){
    if (tau[i]==0){
      sd.mat[j,i]=sqrt(1/sum(1/sigma.s))
    }
    else{
    sd.mat[j,i]=sqrt(1/(1/sigma.s[j]+1/tau[i]^2)+(sum(1/(sigma.s+tau[i]^2)))^(-1)*(sigma.s[j]^2/(tau[i]^2+sigma.s[j]))^2)
    }
  }
}
plot(1,ylim=c(0,0.5),xlim=c(0,0.6),main="sd given tau and y",xlab="tau",ylab="sd")
for(i in 1:length(logodd.s)){
  lines(tau,sd.mat[i,])
  text(tau[length(tau)],sd.mat[i,length(sd.mat[i,])],labels=i)
}
##sampling mu and tau##
mu.tao.pos.fun=function(mu,tau,sigma,y){
  return(
    prod((1/sqrt(sigma^2+tau^2))*exp(-(y-mu)^2/(2*(sigma^2+tau^2))))
  )
}
tau.set=seq(0,0.6,0.001)
mu.set=seq(-2,2,0.005)
mu.len=length(mu.set)
tau.len=length(tau.set)
mu.tau.poster=matrix(rep(NA,mu.len*tau.len),mu.len,tau.len)
for (i in 1:mu.len){
  for (j in 1:tau.len){
    mu.tau.poster[i,j]=mu.tao.pos.fun(mu.set[i],tau.set[j],sigma.s,logodd.s)
  }
}
tau.p.s=rep(NA,1000)
mu.p.s=rep(NA,1000)
theta.p.s=matrix(rep(NA,length(logodd.s)*1000),length(logodd.s),1000)
for (i in 1:1000){
  foo=sample(mu.len,1,prob=apply(mu.tau.poster,1,sum))
  mu.p.s[i]=mu.set[foo]
  tau.p.s[i]=sample(tau.set,1,prob=mu.tau.poster[foo,])
}
#sampling the thetas#
for(i in 1:length(logodd.s)){
  theta.mu=(logodd.s[i]/sigma.s[i]+mu.p.s/tau.p.s^2)/(1/sigma.s[i]+1/tau.p.s^2)
  theta.sigma=1/(1/sigma.s[i]+1/tau.p.s^2)
  theta.p.s[i,]=rnorm(1000,mean=theta.mu,sd=sqrt(theta.sigma))
}
theta.median=apply(theta.p.s,1,median)
theta.median
plot(theta.median~logodd.s,xlim=c(-0.6,0.6),ylim=c(-1,1))
text(logodd.s,theta.median,labels=seq(1,22),pos=2)
abline(0,1,col="red")
for (j in 1:length(logodd.s)){
  lines(x=c(logodd.s[j],logodd.s[j]),y=quantile(theta.p.s[j,],c(0.025,0.975)))
}
abline(mean(logodd.s),0,lty=2)
legend("topleft",lty=c(1,2),legend=c("95%posterior interval","population mean(appro)")) 

#posterior predictive#
theta.new=rnorm(1000,mean=mu.p.s,sd=tau.p.s)
hist(theta.new,breaks=20)
