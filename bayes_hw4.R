Problem 1

Y1=c(16,9,10,13,19,20,18,17,35,55)
Y2=c(58,90,48,57,103,57,86,112,273,64)
theta=Y1/(Y1+Y2)

alpha=seq(0.01,6,0.01)
beta=seq(1,14,0.01)
I=length(alpha)
J=length(beta)
posterior.ab=matrix(rep(NA),I,J)

posterior.ab[i,j]=((alpha[i]+beta[j])^(-2.5))*prod( beta(alpha[i],beta[j])*beta(alpha[i]+Y1,beta[j]+Y2))



for (i in 1:I) {
	for (j in 1:J){

posterior.ab[i,j]=prod(exp((-2.5)*log(alpha[i]+beta[j])+sum(lgamma(alpha[i]+beta[j])-lgamma(alpha[i])-lgamma(beta[j])+lgamma(alpha[i]+Y1)+lgamma(beta[j]+Y2)-lgamma(alpha[i]+beta[j]+Y1+Y2))) )


		}
}
alpha.vector.y=vector()
beta.vector.y=vector()


for (i in 1:1000){
	post.alpha.y=apply(posterior.ab,1,sum)
	foo=sample(length(alpha),1,replace=T,prob=post.alpha.y)
	alpha.sim.y=alpha[foo]
	post.beta.y=posterior.ab[foo,]
	beta.sim.y=sample(beta,1,replace=T,prob=post.beta.y)
	alpha.vector.y=c(alpha.vector.y,alpha.sim.y)
	beta.vector.y=c(beta.vector.y,beta.sim.y)
	
	
}


contour(alpha,beta,posterior.ab*(10^100))

plot(alpha.vector.y,beta.vector.y)

matrix.ab=matrix(rep(NA),10,1000)

for (i in 1:10){
	for (j in 1:1000){
		matrix.ab[i,j]=rbeta(1,alpha.vector.y[j]+Y1[i],beta.vector.y[j]+Y2[i])
	}
}

plot(matrix.ab)



mode=max(posterior.yes)

mu.y=alpha.vector.y/(alpha.vector.y+beta.vector.y)
mu.z=alpha.vector.z/(alpha.vector.z+beta.vector.z)
hist(mu.y-mu.z)

Problem 2

alpha=seq(0.001,9,0.005)
beta=seq(0.001,0.7,0.005)
I=length(alpha)
J=length(beta)
posterior.ab2=matrix(rep(NA),I,J)

for (i in 1:I) {
	for (j in 1:J){
posterior.ab2[i,j]= (beta[j]^(10*alpha[i]))/(gamma(alpha[i])^10)/prod((1+beta[j])^(alpha[i]+Y1)/gamma(alpha[i]+Y1))


		}
}
alpha.vector.y=vector()
beta.vector.y=vector()


for (i in 1:1000){
	post.alpha.y=apply(posterior.ab2,1,sum)
	foo=sample(length(alpha),1,replace=T,prob=post.alpha.y)
	alpha.sim.y=alpha[foo]
	post.beta.y=posterior.ab2[foo,]
	beta.sim.y=sample(beta,1,replace=T,prob=post.beta.y)
	alpha.vector.y=c(alpha.vector.y,alpha.sim.y)
	beta.vector.y=c(beta.vector.y,beta.sim.y)
	
	
}

contour(alpha,beta,posterior.ab2)

Problem 4

data <- read.table(pipe("pbpaste"), sep="\t", header=T)
control.deaths=data[,2]
control.total=data[,3]
treated.deaths=data[,4]
treated.total=data[,5]

y=log(treated.deaths/(treated.total-treated.deaths))-log(control.deaths/(control.total-control.deaths))

sigma=1/treated.deaths+1/(treated.total-treated.deaths)+1/control.deaths+1/(control.total-control.deaths)

mu=seq(0.001,9,0.005)
tau=seq(0.01,0.4,0.01)
I=length(mu)
J=length(tau)
posterior.tau=matrix(rep(NA),1,J)

for (j in 1:J){
mu1=sum(y/(sigma+tau[j]^2))/sum(1/(sigma+tau[j]^2))
posterior.tau[j]= sqrt(1/sum(1/(sigma+tau[j]^2)))*prod(((sigma+tau[j]^2)^(-1/2))*exp(-((y-mu1)^2)/(2*(sigma+tau[j]^2))))
}
plot(tau,posterior.tau,type='l')

theta.mean.matrix=matrix(rep(NA),J,22)
theta.sd.matrix=matrix(rep(NA),J,22)

for (j in 1:J){

for (i in 1:22){
mu1=sum(y/(sigma+tau[j]^2))/sum(1/(sigma+tau[j]^2))
v1=1/sum(1/(sigma+tau[j]^2))
mu.sim=rnorm(1000,mu1,sart(v1)

mu=(y[i]/sigma[i]+mu.sim/tau[j])/(1/sigma[i]+1/tau[j])
v=1/(1/sigma[i]+1/tau[j])

theta.vector=rnorm(1000,mu,v)


theta.mean.matrix[j,i]= mean(theta.vector)
theta.sd.matrix[j,i]= sd(theta.vector)


}
}

matplot(theta.mean.matrix, type = c("l"),pch=1)
matplot(theta.sd.matrix, type = c("l"),pch=1)


tau.sim=sample(tau,1000,replace=T,prob=posterior.tau)
mu.sim=matrix(rep(NA),1,length(tau.sim))

for (j in 1:length(tau.sim)){


mu1=sum(y/(sigma+tau.sim[j]^2))/sum(1/(sigma+tau.sim[j]^2))
v1=1/sum(1/(sigma+tau.sim[j]^2))
mu.sim[j]=rnorm(1,mu1,sqrt(v1))

}  

theta.sim=matrix(rep(NA),1,22)

for (i in 1:22){
mu=(y[i]/sigma[i]+mu.sim/tau.sim^2)/(1/sigma[i]+1/tau.sim^2)
v=1/(1/sigma[i]+1/tau.sim)

theta=median(rnorm(1000,mu,v))
theta.sim[i]=theta
}

for (i in 1:22){
mu=(y[i]/sigma[i]+mu.sim/tau.sim^2)/(1/sigma[i]+1/tau.sim^2)
v=1/(1/sigma[i]+1/tau.sim)

theta=median(rnorm(1000,mu,v))
theta.sim[i]=theta
}


plot(theta.sim[which(treated.total>600)],y[which(treated.total>600)],xlim=c(-0.5,0))
plot(theta.sim[which(treated.total<600)],y[which(treated.total<600)],xlim=c(-0.5,0))


theta.new=rnorm(1000,mean=mu.sim,sd=sqrt(tau.sim))

hist(theta.new)


Problem 6.7
test <- NULL
for (i in 1:1000){
  theta <- rbeta (1,8,14)
  y.rep <- rbinom (1,1,theta)
  while (sum(y.rep==0) < 13)
    y.rep <- c(y.rep, rbinom(1,1,theta))
  n.rep <- length(y.rep)
  test <- c(test,
    sum (y.rep[2:n.rep] != y.rep[1:(n.rep-1)]))}
hist (test, xlab="T (y-rep)", yaxt="n",
  breaks=seq(-.5, max(test)+.5), cex=2)



test =vector()
for (i in 1:1000){
  theta <- rnorm (1,5.1,1/sqrt(100))
  y.rep <- rnorm (100,theta,1)
  test <- c(test,max(abs(y.rep)))
  }
postscript ("fig6.6a.ps", horizontal=TRUE)
par (mar=c(5,5,4,1)+.1)
hist (test, xlab="T (y-rep)", yaxt="n",
  nclass=20, cex=2)
lines (rep(8.1,2), c(0,1000))
print (mean(test>8.1)) 


















