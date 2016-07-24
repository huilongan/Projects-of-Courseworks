str(score)
summary(score)
dim(score)
ques=score[,6:33]
num_score=score[,5:33]
#outlier
#md=mahalanobis(ques,cov=cov(ques),center=apply(ques,2,mean))
library(car)
#scatterplotMatrix(ques)
#qqplot(qchisq(ppoints(length(md)),df=28),md,main="chi-square outlier detector")
#abline(0,1,col="red")
#par(mfrow=c(1,3))
#for (i in unique(score$instr)){
#  in_data=score[score$instr==i,6:33]
#  mds=mahalanobis(in_data,cov=cov(in_data),center=apply(in_data,2,mean))
#  qqplot(qchisq(ppoints(length(mds)),df=28),mds,main=expression("chi-square outlier detector for instr"~~i))
#  abline(0,1,col="red")
#  }
boxplot(num_score)
which.min(score$Q17)
num_score[5755,]
library(mvoutlier)
aq.plot(ques,delta=qchisq(0.975,df=3),quan=0.5,alpha=0.05)

##factor analysis
library(lattice)
levelplot(cor(ques))
levelplot(cor(num_score))
faa=factanal(ques,factors=2,method="mle",rotation="varimax",scores="regression")
faa$loadings
sapply(1:4,function(f) factanal(ques,factors=f,method="mle",rotation="varimax")$PVAL)
fl=cbind(faa$loadings[,1],faa$loadings[,2])
plot(fl[,1]~fl[,2],xlim=c(0,1),ylim=c(0,1),xlab="factor2",ylab="factor1")
for (i in 1:28){
  arrows(0,0,fl[i,2],fl[i,1])
}
text(fl[,2],fl[,1],labels=names(ques),col="red",cex=0.7)
abline(0,1,col="blue")
#pr.out=princomp(ques,cor=F)
#biplot(pr.out)
#summary(pr.out,loadings=T)
#tcrossprod+diag term is the predicted correlation matrix and cor() is the observed cor mat
max(abs(round(tcrossprod(faa$loadings)+diag(faa$uniquenesses)-cor(ques),3)))
#min correlation in the observed cor mat
min(cor(ques))
names(faa)
scores_f=faa$scores
plot(scores_f)
identify(scores_f,col.index="red")
arrows(-4,4,4,-4,col="red",lwd=3)
text(-4,4,lables="trend",col="red")
dotchart(score$difficulty)
dotchart(ques$Q1)
##verify
par(mfrow=c(1,2))
ques12_mean=apply(ques[,1:12],1,mean)# mean of 1:12 question
ques28_mean=apply(ques[,13:28],1,mean)# mean of 13:18 question
plot(ques12_mean~scores_f[,2],xlab="2nd factor score",ylab="mean of 1:12 ques scores")
arrows(-2,20,2,100,col="red",lwd=3)
plot(ques28_mean~scores_f[,1],xlab="1nd factor score",ylab="mean of 13:28 ques scores")
arrows(-2,20,2,100,col="red",lwd=3)
plot(ques[,1]~scores_f[,2])
levelplot(cor(data.frame(scores_f[,1],ques[,13:28])))
par(mfrow=c(4,4))
for (i in 13:28){
  plot(scores_f[,1]~ques[,i],axes=F,xlab="question varibale",ylab="first factor score",ylim=c(-4,4),xlim=c(0,100),col=i,pch=16)
  lines(lowess(data.frame(ques[,i],scores_f[,1])),col=i+1,lwd=2)
}
#difficulty versus score
diff_scaled=scale(score$difficulty)
lm.fit=lm(diff_scaled~scores_f[,1]+scores_f[,2])
levelplot(cor(data.frame(difficulty=diff_scaled,instructor.e=scores_f[,1],course.e=scores_f[,2])))
pairs(num_score)

course.e=cut(scores_f[,2],5)
levels(course.e)=c(1,2,3,4,5)
instr.e=cut(scores_f[,1],5)
levels(instr.e)=seq(1:5)  
library(vcd)  
cotabplot(~diff.f+course.e|instr.e,shade=TRUE,legend=TRUE)
cotabplot(~diff.f+instr.e|course.e,shade=TRUE,legend=TRUE)
diff.f=cut(score$difficulty,5)
levels(diff.f)=c(1,2,3,4,5)
plot(scores_f[,1],scores_f[,2],col=score$instr,pch=16)
#radius=sqrt(score$difficulty/pi)
symbols(scores_f[,1],scores_f[,2],circles=radius,inches=0.35,fg=score$class,bg=score$instr,xlab="evaluation on instructor",
        ylab="evaluation on courses")
#library("scatterplot3d")
#with(score,scatterplot3d(scores_f[,1],scores_f[,2],difficulty,type="h",angle=55 ))
##within category group
names(score)

mosaic(~score$nb.repeat+score$attendance,shade=TRUE,legend=TRUE)

cor(score$difficulty,score$nb.repeat)
course1=score[score$class==1,]
library(MASS)
lda.fit=lda(score$diff_level~scores_f[,1]+scores_f[,2])
lda.fit
plot(lda.fit)
boxplot(scores_f[,2]~score$instr)
boxplot(score$difficulty~score$class)
course10=score[score$class==10,]
fix(course10)
names(score)
chisq.test(score$diff_level,score$instr)
chisq.test(score$instr,score$nb.repeat)

##between variable group
boxplot(score$difficulty~score$instr+score$class)
lm.fit=lm(scores_f[,1]~as.factor(score$instr)+as.factor(score$class)+as.factor(score$nb.repeat)+as.factor(score$attendance))
anova(lm.fit)
summary(lm.fit)
score[score$class==10,]
lm.fit2=lm(scores_f[,1]~as.factor(score$instr))
anova(lm.fit2)
score$difflevel=diff.f
par(mfrow=c(1,2))
mosaic(~score$instr+score$difflevel,shade=TRUE,legend=TRUE)
boxplot(score$difficulty~score$instr)
mosaic(~score$class+score$difflevel,shade=TRUE,legend=TRUE)
boxplot(score$difficulty~score$class)
boxplot(scores_f[,1]~score$class)##test

lm.fit=lm(scores_f[,2]~as.factor(score$attendance)+as.factor(score$instr)+as.factor(score$class)+as.factor(score$nb.repeat))
summary(lm.fit)
anova(lm.fit)
boxplot(scores_f[,2]~score$class)



## problem2
data=data.frame(a1_raw,a1_va3)
data[1744:(1744+1259),]=data.frame(a2_raw,a2_va3)
data[3004:(3004+1829),]=data.frame(a3_raw,a3_va3)

#k-means
s_data=scale(data)
wss=rep(0,8)
wss[1]=(nrow(s_data)-1)*ncol(s_data)
for(i in 2:8){
  wss[i]=sum(kmeans(s_data,centers=i)$withiness)
}
plot(1:8,wss,type="b",xlab="N of group",ylab="within ss")
lable=kmeans(s_data,centers=5)$cluster
pcs.score=princomp(data,cor=T)$scores
plot(pcs.score[,1],pcs.score[,2],col=lable,pch=16)
legend(locator(),pch=16,col=1:5,legend=c("group1","group2","group3","group4","group5"))
##h cluster
h.clust=hclust(dist(s_data),method="complete")
plot(h.clust,hang=-1)
cov.m=cov(data)
md=mahalanobis(data,center=apply(data,2,mean),cov=cov.m)

qqplot(qchisq(ppoints(length(md)),df=50),md,main="chi-square outlier detector")
abline(0,1,col="red")
lable_c=kmeans(scale(use.data),centers=5)$cluster
pcs.score=princomp(use.data2,cor=T)$scores
summary(princomp(use.data2,cor=T),loadings=T)
plot(pcs.score[,1],pcs.score[,2],col=lables,pch=16)
legend(locator(),pch=16,col=1:5,legend=c("group1","group2","group3","group4","group5"))

use.data.s=scale(use.data)
h.clust=hclust(dist(use.data.s))
plot(h.clust,hang=-1)
########
###different data set
use.data4=use.data[,c(1:18)]##only the position
use.data5=use.data[,c(1:42)]##the position and the vectorial velocity
use.data6=data[,-19]##postion and vectorial velocity and scalar velocity
use.data7=data[,-c(1:19)]##only the vectorial velocity and scalar velocity
use.data3=use.data[,-c(19:42)]##the position and the scalar velocity
## set a function: pc score plot, kmeans
kmeans.c=function(data,center){
print(summary(princomp(data[,-51],cor=T),loading=T))
biplot(princomp(data[,-51],cor=T))
print(princomp(data[,-51],cor=T)$loadings[,1])
score.pc=princomp(data,cor=T)$score
labels=kmeans(scale(data),centers=center)$cluster
table(labels)
plot(score.pc[,1]~score.pc[,2],col=labels,pch=16)
data[,51]=labels
}
###position difference of 5 group
par(mfrow=c(4,5))
for(i in 1:18){
boxplot(use.data6[,i]~use.data6$cluster,main=names(use.data6)[i])
}
###"remove" process
kmeans.c(use.data6,5)
group1=which(use.data6$cluster==5)
use.data5.n5=use.data6[use.data6$cluster!=5,]
kmeans.c(use.data5.n5,4)
group2=as.numeric(row.names(use.data5.n5[which(use.data5.n5$cluster==3),]))
use.data6.n3=use.data5.n5[use.data5.n5$cluster!=3,]
kmeans.c(use.data6.n3,3)
length(which(use.data6.n3$cluster==4))
group3=as.numeric(row.names(use.data6.n3[which(use.data6.n3$cluster==2),]))
kmeans.c(use,2)
group4=as.numeric(row.names(use[which(use$cluster==1),]))

