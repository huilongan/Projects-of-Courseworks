Prlblem 3.12
Year=seq(1976,1985,1)
Fatal=c(24,25,31,31,22,21,26,20,16,22)
Passanger=c(734,516,754,877,814,362,764,809,223,1066)
death=c(0.19,0.12,0.15,0.16,0.14,0.06,0.13,0.13,0.03,0.15)

mu=c(-1,1850)
sigma=matrix(c(0.01,-0.5,-0.5,100),2,2)
mv=mvrnorm(1000,mu,sigma)
plot(mv,main='prior.scatterplot')                              
###simulate the bivariate normal prior dist data and plot the prior contour map

bi.kde=kde2d(mv[,1],mv[,2],n=100)
contour(bi.kde,main='prior.contour')  



lm1=lm(Fatal~Year)



alpha=seq(-1000,6000,10)
beta=seq(-2,-1,0.001)
I=length(alpha)
J=length(beta)
posterior=matrix(rep(NA),I,J)
for (i in 1:I) {
	for (j in 1:J){
		lambda=alpha[i]+beta[j]*Year
		if (sum(lambda<=0)==0) {
		posterior[i,j]=prod(exp(-lambda)*((lambda^Fatal)/factorial(Fatal)))
		} else {
			posterior[i,j]=0
		}
		
				
}
}
alpha.vector=vector()
beta.vector=vector()                 ##posterior using uniform as prior
contour(alpha,beta,posterior,main='posterior.new.contour')


for (i in 1:1000){
	post.alpha=apply(posterior,1,sum)
	foo=sample(length(alpha),1,replace=T,post.alpha)
	alpha.sim=alpha[foo]
	post.beta=posterior[foo,]
	beta.sim=sample(beta,1,replace=T,prob=post.beta)
	alpha.vector=c(alpha.vector,alpha.sim)
	beta.vector=c(beta.vector,beta.sim)
}
hist(alpha.vector+1986*beta.vector,breaks=40)
expect.fatal=alpha.vector+beta.vector*1986

interval.95=sort(expect.fatal)[c(26,975)] #### 95% interval
