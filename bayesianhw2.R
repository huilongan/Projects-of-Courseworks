getwd()
setwd("c:/users/an/desktop")
data=read.table("lol.txt")
plot(table(light),type="h")
hist(light,breaks=40)
sigma1=rchisq(1000,65)
s2=66*var(light)/(65)
sigma=65*s2/sigma1
mu=NULL;
for (i in 1:1000){
mu[i]=rnorm(1,mean=mean(light),sd=sqrt(sigma[i]));
}
set.seed(101)
smuc=rt(1000,31)
muc=(0.24*smuc+1.013)/sqrt(32)
smutmc=rt(1000,35)
mutmc=(0.2*smutmc+1.173-muc)/sqrt(36)
#the false process#
tabmuc=as.data.frame(table(muc))
tabmutmc=as.data.frame(table(mutmc))
tabmuc$prob=tabmuc$Freq/1000
tabmutmc$prob=tabmutmc$Freq/1000
po.prob=tabmuc$prob*tabmutmc$prob
po.dist=data.frame(mutmc,po.prob)

#this right process#
cut.muc=cut(muc,100)
cut.mutmc=cut(mutmc,100)
tablemutmc=as.data.frame(table(cut.mutmc))
tablemutmc$prob=tablemutmc$Freq/sum(tablemutmc$Freq)
tablemuc=as.data.frame(table(cut.muc))
tablemuc$prob=tablemutmc$Freq/sum(tablemuc$Freq)
poster.prob=tablemuc$prob*tablemutmc$prob
poster.dist=data.frame(tablemutmc$cut.mutmc,poster.prob)
names(poster.dist)=c("mtmc","unprob")
#nomalize the P.D.F#
length=(max(mutmc)-min(mutmc))/100
total=0;
for(i in 1:100){
total=total+length*poster.dist$unprob[i];
}
total
poster.dist$prob=poster.dist$unprob*(1/total)
fix(poster.dist)
sum=0;
for (i in 1:100){
sum=sum+poster.dist$prob[i]*length;
if(sum>=0.025) {lower=min(mutmc)+i*length;break;}
}
sum=0;
for (i in 1:100){
sum=sum+poster.dist$prob[i]*length;
if(sum>=0.975) {upper=min(mutmc)+i*length;break;}
}
lower;
upper;
plot(prob~seq(min(mutmc),max(mutmc),(max(mutmc)-min(mutmc))/99),data=poster.dist,type='s')
par(new=T)
plot(prob~seq(min(mutmc),max(mutmc),(max(mutmc)-min(mutmc))/99),data=poster.dist,type='h')
###########3.3#########
#NUMy#
set.seed(1001)
numy=16+9+10+13+19+20+18+17+35+55
numx=12+1+2+4+9+7+9+8
totaly=16+58+9+90+10+48+13+57+19+103+20+57+18+86+17+112+35+273+55+64
totalx=12+113+1+18+2+14+4+44+9+208+7+67+9+29+8+154
sampley=rbeta(1000,numy+1,totaly+1-numy)
samplex=rbeta(1000,numx+1,totalx+1-numx)
simul=data.frame(sampley,samplex)
head(simul)
y=c(16/(16+58),9/(9+90),10/(10+48),13/(13+57),19/(19+103),
    20/(20+57),18/(18+86),17/(17+112),35/(35+273),55/(55+64))
x=c(12/(12+113),1/(1+18),2/(2+14),4/(4+44),9/(9+208),7/(67+7),
          9/(9+29),8/(8+154))
# sample prior : not bayesian #
meany=mean(sampley)
meanx=mean(samplex)
vary=var(sampley)
varx=var(samplex)
alpha=function(m,v){(m-m^2-m*v)/v}
beta=function(m,v){((m-m^2-v*m)*(1-m))/(v*m)}
#muy-myx simulation#
differ=simul$sampley-simul$samplex
diff=cut(diff,100)
poster=as.data.frame(table(diff))
fix(poster)
poster$unprob=poster$Freq/sum(poster$Freq)
length=(max(differ)-min(differ))/100
poster$prob=poster$unprob*(1/sum(length*poster$unprob))
plot(poster$prob~seq(min(differ),max(differ),(max(differ)-min(differ))/99),type="s")
par(new=T)
plot(poster$prob~seq(min(differ),max(differ),(max(differ)-min(differ))/99),type="h")

### toxic experiment ###
x1=c(-.8,5,0)
x2=c(-.3,5,1)
x3=c(-.05,5,3)
x4=c(0.73,5,5)
data=rbind(x1,x2,x3,x4)
colnames(data)=c("x","n","y")
data=as.data.frame(data)
fix(data)
pos=function(alpha,beta){
    exp(-(1/1.5)*(alpha^2/4+alpha*(beta-10)/20+(beta-10)^2/100))*
    (exp(alpha+beta*(-0.3)))^1*(exp(alpha+beta*(-0.5)))^3+(exp(alpha+beta*0.73))^5/
    ((1+exp(alpha+beta*(-0.8)))*(1+exp(alpha+beta*(-0.3)))*(1+exp(alpha+beta*(-0.05)))+(1+exp(alpha+beta*0.73)))^5
}
xaxis=seq(-5,10,0.05)
yaxis=seq(-10,40,0.05)
x=c(-0.86,-0.3,-0.05,0.73)
n=c(5,5,5,5)
y=c(0,1,3,5)
z=matrix(rep(0),length(xaxis),length(yaxis));
for (i in 1:length(xaxis)){
    for (j in 1:length(yaxis)){
         theta=inv.logit(xaxis[i]+yaxis[j]*x);
         z[i,j]=dmvnorm(c(xaxis[i],yaxis[j]),mean=c(0,10),sigma)*prod((theta^y)*((1-theta)^(n-y)));
          }
}
contour(xaxis,yaxis,z,nlevels=10)  # posterior contour plot#       
#by Gelman suggestion#
pos.g=exp(log(z)-max(log(z)))
contour(xaxis,yaxis,pos.g)
#sampling from the posterior distribution#
ss.alpha=NULL;
ss.beta=NULL;
for (i in 1:1000){
index.a=sample(length(xaxis),1,replace=T,apply(pos.g,1,sum))
s.alpha=xaxis[index.a]
s.beta=sample(yaxis,1,replace=T,pos.g[index.a,])
ss.alpha=c(ss.alpha,s.alpha)
ss.beta=c(ss.beta,s.beta)
}
plot(ss.alpha,ss.beta)
z=cbind(xaxis,yaxis,pos(xaxis,yaxis))
length(xaxis)
length(yaxis)
set=NULL;
for (i in 1:length(xaxis)){
set=rbind(set,cbind(rep(xaxis[i],length(yaxis)),yaxis));
}
z=pos(set$V1,set$yaxis)
set$z=z
max=max(z)
#plot the prior distribution#
library(MASS)
mu=c(0,10)
sigma=matrix(c(4,10,10,100),2,2)
mv=mvrnorm(100,c(0,10),sigma)
plot(mv)
contour(kde2d(mv[,1],mv[,2],n=100))
# the applied context#
ld50=-ss.alpha/ss.beta
hist(ld50,breaks=40)

####iteration to get MLE (equivalent)###
y=c(43,44,45,46.5,47.5)
f=function(theta) sum((y-theta)/(1+(y-theta)^2))  
theta=seq(-100,100)
f.value=NULL;
for (i in 1:length(theta)) { f.value[i]=f(theta[i]);}
f.value
plot(f.value~theta)
info=function(theta) sum(((y-theta)^2-1)/(1+(y-theta)^2)^2);
ori=1;
new=NULL;
step=0;
appro=NULL;
#Newton's iteration# 
for(i in 2:10000)
{
new[1]=0;
new[i]=new[i-1]-f(new[i-1])/info(new[i-1]);
step=step+1;
if (abs(f(new[i])-f(new[i-1]))<=0.001) {appro=new[i];print(new[i]);break;} 
}
step
appro
#the observed information#
ob.info=info(appro)
appro.norm=rnorm(1000,appro,1/ob.info)
hist(appro.norm,breaks=30)
use2app=seq(min(appro.norm),max(appro.norm),(max(appro.norm)-min(appro.norm))/39)
pos=NULL;
pos.form=function(theta) sum(1/(1+(y-theta)^2))
for (i in 1:length(use2app)){
     pos[i]=pos.form(use2app[i]);
}
pos=exp(log(pos)-max(log(pos)))
plot(pos~use2app,type="h")
par(new=T)
plot(pos~use2app,type="s")