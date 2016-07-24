Problem 2
mu.c=1.013+(0.24/sqrt(32)*rt(1000,31))
mu.t=1.173+(0.2/sqrt(36)*rt(1000,35))
difference=mu.t-mu.c
hist(difference,xlab='difference')
print(sort(difference))[c(25,976)]           ##confidence interval

Prlblem 4
Y1=c(16,9,10,13,19,20,18,17,35,55)
Y2=c(58,90,48,57,103,57,86,112,273,64)
N1=c(12,1,2,4,9,7,9,8)
N2=c(113,18,14,44,208,67,29,154)
yes=Y1/(Y1+Y2)
no=N1/(N1+N2)

alpha=seq(0,1,0.01)
beta=seq(0,1,0.01)
I=length(alpha)
J=length(beta)
posterior.yes=matrix(rep(NA),I,J)
posterior.no=matrix(rep(NA),I,J)

for (i in 1:I) {
	for (j in 1:J){
		posterior.yes[i,j]=prod((yes^(alpha[i]-1))*((1-yes)^(beta[j]-1)))
		posterior.no[i,j]=prod((no^(alpha[i]-1))*((1-no)^(beta[j]-1)))
}
}
alpha.vector.y=vector()
beta.vector.y=vector()
alpha.vector.z=vector()
beta.vector.z=vector()

for (i in 1:1000){
	post.alpha.y=apply(posterior.yes,1,sum)
	foo=sample(length(alpha),1,replace=T,prob=post.alpha.y)
	alpha.sim.y=alpha[foo]
	post.beta.y=posterior.yes[foo,]
	beta.sim.y=sample(beta,1,replace=T,prob=post.beta.y)
	alpha.vector.y=c(alpha.vector.y,alpha.sim.y)
	beta.vector.y=c(beta.vector.y,beta.sim.y)
	
	post.alpha.z=apply(posterior.no,1,sum)
	foo=sample(length(alpha),1,replace=T,prob=post.alpha.z)
	alpha.sim.z=alpha[foo]
	post.beta.z=posterior.no[foo,]
	beta.sim.z=sample(beta,1,replace=T,prob=post.beta.z)
	alpha.vector.z=c(alpha.vector.z,alpha.sim.z)
	beta.vector.z=c(beta.vector.z,beta.sim.z)
	
}
mode=max(posterior.yes)

mu.y=alpha.vector.y/(alpha.vector.y+beta.vector.y)
mu.z=alpha.vector.z/(alpha.vector.z+beta.vector.z)
hist(mu.y-mu.z)





Problem 5
x=c(-0.86,-0.3,-0.05,0.73)
n=c(5,5,5,5)
y=c(0,1,3,5)                          ###imput the original data

alpha=seq(-5,10,0.05)
beta=seq(-10,40,0.05)
I=length(alpha)
J=length(beta)
posterior=matrix(rep(NA),I,J)
for (i in 1:I) {
	for (j in 1:J){
		theta=inv.logit(alpha[i]+beta[j]*x)
		posterior[i,j]=prod((theta^y)*((1-theta)^(n-y)))
}
}
alpha.vector=vector()
beta.vector=vector()                 ##posterior using uniform as prior

for (i in 1:1000){
	post.alpha=apply(posterior,1,sum)
	foo=sample(length(alpha),1,replace=T,post.alpha)
	alpha.sim=alpha[foo]
	post.beta=posterior[foo,]
	beta.sim=sample(beta,1,replace=T,prob=post.beta)
	alpha.vector=c(alpha.vector,alpha.sim)
	beta.vector=c(beta.vector,beta.sim)
}
mode=max(posterior)
plot(alpha.vector,beta.vector,main='likelihood.scatterplot')
contour(alpha,beta,posterior,main='likelihood.contour')       
##sampleing 1000 data from joint posterior and plot


mu=c(0,10)
sigma=matrix(c(4,10,10,100),2,2)
mv=mvrnorm(100,c(0,10),sigma)
plot(mv,main='prior.scatterplot')                              
###simulate the bivariate normal prior dist data and plot the prior contour map

bi.kde=kde2d(mv[,1],mv[,2],n=100)
contour(bi.kde,main='prior.contour')                                         

alpha=mv[,1]
beta=mv[,2]
alpha=alpha[order(alpha)]
beta=beta[order(beta)]
I=length(alpha)
J=length(beta)
posterior=matrix(rep(NA),I,J)        

for (i in 1:I) {
	for (j in 1:J){
		theta=inv.logit(alpha[i]+beta[j]*x)
		posterior[i,j]=dmvnorm(c(alpha[i],beta[j]),mean=c(0,10),sigma)*prod((theta^y)*((1-theta)^(n-y)))
}
}
alpha.vector=vector()
beta.vector=vector()                  ###get the posterior using new prior

for (i in 1:10000){
	post.alpha=apply(posterior,1,sum)
	foo=sample(length(alpha),1,replace=T,post.alpha)
	alpha.sim=alpha[foo]
	post.beta=posterior[foo,]
	beta.sim=sample(beta,1,replace=T,prob=post.beta)
	alpha.vector=c(alpha.vector,alpha.sim)
	beta.vector=c(beta.vector,beta.sim)
}                                     ###sampling 10000 data from joint posterior and plot

plot(alpha.vector,beta.vector,ylim=c(-10,30),xlim=c(-4,4),main='posterior.new.scatterplot')
contour(alpha,beta,posterior,main='posterior.new.contour')
par(mfrow=c(3,2))







LD50=-alpha.vector/beta.vector       ### draw the histogram of LD50
hist(LD50,breaks=20)





Problem 6
y=c(-2,-1,0,1.5,2.5)
theta=0.9
theta.vector=vector()
for (i in 1:10000){
l=-sum(2*(theta-y)/(1+(y-theta)^2))
lambda=0.001
theta=theta+lambda*l
l1=-sum(2*(theta-y)/(1+(y-theta)^2))
theta.vector=c(theta.vector,theta)
if (abs(l1-l)<0.000001) {
	break}
}                                 ### using the gradient descent to get theta

fisher=2*sum(((mode-y)^2-1)/(1+(mode-y)^2))   ###compute the fisher information

y6 <- rcauchy (length(theta.true.vector), theta.true.vector, 1)
par(mfrow=c(1,2))
hist(y6,breaks=20,main='exact.cauchy')
hist(rnorm(100,-1.4,sqrt(1/1.29)),main='appro.cauchy') 
####compare exact distribution with approximate distribution
theta




