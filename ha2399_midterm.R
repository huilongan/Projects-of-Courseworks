#question1#
# initial exploration about the data#
str(data1)# it's a "list"
attributes(data1)# the attributes of this list
library(scatterplot3d)
data=as.data.frame(data1)#turn a list into dataframe
par(mfrow=c(3,3))#take a look at the crude distribution of the variables#
for (i in unique(data$group)){
  boxplot(data[data$group==i,]$x,main="x")
  boxplot(data[data$group==i,]$y,main="y")
  boxplot(data[data$group==i,]$z,main="z")
}
# x,z's distribution are very plain. y has some extreme values#
par(mfrow=c(1,3))#show three variables respectively
for (i in unique(data$group)){
  show=c("group",i)
#  scatterplot3d(data[data$group==i,]$x,data[data$group==i,]$y,
#                data[data$group==i,]$z,pch=1,color=i,main=show)
  l.m=lm(data[data$group==i,]$z~data[data$group==i,]$x+data[data$group==i,]$y)
  print(summary(l.m))
  scatterplot3d(data[data$group==i,]$x,data[data$group==i,]$y,
                data[data$group==i,]$z,pch=16,main=show,color=i+5,type="h")$plane3d(l.m,lty.box="dashed")
}




#we can find their respective structure in 3d space are very similar#
#show in a plot to see the structure of these three variables in 3-d space#    
scatterplot3d(data$x,data$y,data$z,color=data$group,pch=data$group)
#rotate it to see it more clear#
library(plyr)#we need a increasing sequence before using persp#
orderx=arrange(data,data$x)$x
ordery=arrange(data,data$y)$y
data.m=matrix(rep(NA),length(orderx),length(ordery))
col.m=matrix(rep(NA),length(orderx),length(ordery))
p.x=order(data$x)
p.y=order(data$y)
for(i in 1:length(p.x)){
  for(j in 1:length(p.y)){
    if(p.x[i]==p.y[j]){
      data.m[i,j]=data[p.x[i],]$z
      col.m[i,j]=data[p.x[i],]$group
    }
  }
}

#for (i in 1:length(orderx)){
#    for (j in 1:length(ordery)){
#          for (k in dim(data)[1]){
#             if (data[k,]$x==orderx[i]&data[k,]$y==ordery[j])
#                    { data.m[i,j]=data[k,]$z;
#                      col.m[i,j]=data[k,]$group;
#                      break;
#                    }
#                                  }
#                               }
#                          }
library("prodlim")
#for (i in 1:length(orderx)){
#   for (j in 1:length(ordery)){
#         row=row.match(c(orderx[i],ordery[j]),data)
#         if(is.na(row)==FALSE)
#         {data.m[i,j]=data[row,]$z;col.m[i,j]=data[row,]$group;}
#   }
#  }


#persp(orderx,ordery,data.m,col=col.m)
#rotate 3d scatterplot#
scatter3D(data$x,data$y,data$z,pch=data$group,col=data$group,phi=40,theta=30)
#projection#
pmat=scatter3D(data$x,data$y,data$z,pch=data$group,col=data$group)
xy=trans3D(data$x,data$y,data$z,pmat=pmat)
lines(xy,col="grey")
pmat.2=perspbox(z=diag(2))
xy.2=trans3D(x=data$x,y=data$y,z=data$z,pmat=pmat.2)
polygon(xy.2)
scatter3D(data$x,data$y,data$z,pch=data$group,col=data$group,phi=90,theta=0)
scatter3D(data$x,data$y,data$z,pch=data$group,col=data$group,phi=0,theta=90)
scatter3D(data$x,data$y,data$z,pch=data$group,col=data$group,phi=180,theta=45)
scatter3D(data$x,data$y,data$z,pch=data$group,col=data$group,phi=180,theta=30)
#princomp#
pr.out=princomp(data[,-4],cor=T)
summary(pr.out,loadings=T)
var=(pr.out$sdev)^2
par(mfrow=c(1,2))
plot(var~seq(1,3),type="b")
plot(cumsum(var)~seq(1,3),type="b")
text(seq(1,3),cumsum(var),labels=round(cumsum(var)/sum(var),3))
biplot(pr.out)
score=as.data.frame(pr.out$scores)
score$col=data$group
plot(score[,1]~score[,2],col=score$col)
#projection#
mu=apply(data[,1:3],2,mean)
data.center=scale(data[,1:3],scale=F)
par(mfrow=c(1,2))
for (i in 1:2){
  name=c("PC",i)
  project=data.center%*%outer(pr.out$loadings[,i],pr.out$loadings[,i])+
    matrix(mu,nrow=nrow(data.center),ncol=length(mu),byrow=T)
  project=as.data.frame(project)
  project$group=data$group
  scatterplot3d(project$x,project$y,project$z,color=project$group,axis=F,pch=16,main=name)
}
#contour plot by#
library(mvtnorm)
library(KernSmooth)
contour.plot=function(x,y,z,...){
  ks=bkde2D(cbind(x,y),bandwidth=c(sd(x),sd(y))/2)
  contour(ks$x1,ks$x2,ks$fhat,add=T,nlevels=5,col=z,lwd=5)
}
plot(score[,1]~score[,2],col=score$col,pch=16)
for (i in unique(score$col)){
  group=score[score$col==i,]
  contour.plot(group[,1],group[,2],i+4);
}
legend("topleft",inset=0.05,lty=1,col=c(5,6,7),legend=c("group1","group2","group3"))
#group=score[score$col==1,]
#ks=bkde2D(cbind(group[,1],group[,2]),bandwidth=c(sd(group[,1]),sd(group[,2]))/2)
#contour(ks$x1,ks$x2,ks$fhat,add=T,nlevels=5)
library("ggplot2")
library("devtools")
library("ggbiplot")
str(data1$group)
gg=ggbiplot(pr.out,obs.scale=1,var.scale=1,groups=as.factor(data$group),
            ellipse=TRUE,circle=TRUE)+scale_color_discrete(name='')+theme(
              legend.direction='vertical',legend.position='right')+ggtitle(
                "data on first-two-PCs-space ")
print(gg)
##problem 2 #####################

data2=as.data.frame(data2)
names(data2)=c("x","y","z","group")
library(scatterplot3d)
library(plot3D)
scatter3D(data2$x,data2$y,data2$z,col=data2$group,pch=16)
scatterplot3d(data2$x,data2$y,data2$z,color=data2$group,pch=data2$group)
#scatter3D(data2$x,data2$y,data2$z,pch=16,col=data2$group,phi=90,theta=0)
#scatter3D(data2$x,data2$y,data2$z,pch=16,col=data2$group,phi=60,theta=30)
#unique(data2$group)
#par(mfrow=c(1,3))
#for (i in 1){
#  len=300
#  name=c("group",i)
#  l.l=loess(data2[data2$group==i,]$z~data2[data2$group==i,]$x+data2[data2$group==i,]$y)
#  grid=data.frame(x=seq(min(data2[data2$group==i,]$x),max(data2[data2$group==i,]$x),length.out=length(data2[data2$group==i,]$x)),
#                  y=seq(min(data2[data2$group==i,]$y),max(data2[data2$group==i,]$y),length.out=length(data2[data2$group==i,]$y)))
#  xy=expand.grid(x=grid$x,y=grid$y)
#  l.pred=matrix(predict(l.l,newdata=xy),nrow=length(data2[data2$group==i,]$x),ncol=length(data2[data2$group==i,]$x))
#  fitpoints=predict(l.l)
#  surf=list(grid$x,grid$y,z=l.pred,facets=NA,fit=fitpoints)
#  scatter3D(data2[data2$group==i,]$x,data2[data2$group==i,]$y,data2[data2$group==i,]$z,pch=16,col=i,main=name,surf=surf)
#}

for (i in 2){
  len=100
  name=c("group",i)
  #l.l=loess(data2[data2$group==i,]$z~data2[data2$group==i,]$x+data2[data2$group==i,]$y)
  #grid=data.frame(x=seq(min(data2[data2$group==i,]$x),max(data2[data2$group==i,]$x),length.out=len),
  #               y=seq(min(data2[data2$group==i,]$y),max(data2[data2$group==i,]$y),length.out=len))
  #xy=expand.grid(x=grid$x,y=grid$y)
  #l.pred=matrix(predict(l.l,newdata=xy),nrow=len,ncol=len)
  #fitpoints=predict(l.l)
  #surf=list(grid$x,grid$y,z=l.pred,facets=NA,fit=fitpoints)
  scatter3D(data2[data2$group==i,]$x,data2[data2$group==i,]$y,data2[data2$group==i,]$z,pch=16,col=i,main=name,type="h")
  par(new=TRUE)
  scatter3D(data2[data2$group==i,]$x,data2[data2$group==i,]$y,data2[data2$group==i,]$z,pch=16,col=i,main=name,type="l")
}
par(mfrow=c(1,3))
data22=data2[data2$group==2,]
scatter3D(data22[order(data22$x),]$x,data22[order(data22$x),]$y,data22[order(data22$x),]$z,type="p")
data21=data2[data2$group==1,]
scatter3D(data21[order(data22$x),]$x,data21[order(data22$x),]$y,data21[order(data22$x),]$z,type="p")
data23=data2[data2$group==3,]
scatter3D(data23[order(data22$x),]$x,data23[order(data22$x),]$y,data23[order(data22$x),]$z,type="p")

par(mfrow=c(3,3))#take a look at the crude distribution of the variables#
for (i in unique(data2$group)){
  boxplot(data2[data2$group==i,]$x,main="x")
  boxplot(data2[data2$group==i,]$y,main="y")
  boxplot(data2[data2$group==i,]$z,main="z")
}
library(car)
#md=function(x){
#  x=as.matrix(x)
#  mu=apply(x,2,mean)
#  sigma=cov(x)
#  md=apply(x,1,function(x) (x-mu)%*%solve(sigma)%*%t(x-mu))
#  return(md)
#}
md=mahalanobis(data2[,1:3],center=apply(data2[,1:3],2,mean),cov=cov(data2[,1:3]))
#md(data2[,1:3])
qqplot(qchisq(ppoints(length(md)),df=3),md,
       xlab="theoretical quan",ylab="sample quan")
abline(a=0,b=1,col="red")
#outlierness#
library(mvtnorm)
library(mvoutlier)
aq.plot(data2[,1:3],delta=qchisq(0.975,df=3),quan=0.5,alpha=0.05)
check=aq.plot(data2[,1:3],delta=qchisq(0.975,df=3),quan=0.5,alpha=0.05)$outliers
for (i in 1:length(check)){
  if(check) {pirnt(i)}
}
unique(check)
pr.out=princomp(data2[,1:3],cor=T)
summary(pr.out,loadings=T)
var=(pr.out$sdev)^2
par(mfrow=c(1,2))
plot(var~seq(1,3),type="b")
plot(cumsum(var)~seq(1,3),type="b")
text(seq(1,3),cumsum(var),labels=round(cumsum(var)/sum(var),3))
biplot(pr.out)
score=as.data.frame(pr.out$scores)
score$col=data2$group
plot(score[,2]~score[,1],col=score$col,pch=16)
par(new=T)
scatterplot3d(data2$x,data2$y,data2$z,color=data2$group,pch="*")$points3d(mean(data2$x),
                                                                          mean(data2$y),
                                                                          mean(data2$z),
                                                                          col="yellow",type="h",pch=16,lwd=3)
par(new=T)
scatterplot3d(data2$x,data2$y,data2$z,color=data2$group,pch="*")$points3d(c(mean(data2$x),7),
                                                                          c(mean(data2$y),7),
                                                                          c(mean(data2$z),0),
                                                                          col="blue",type="l",pch=16,lwd=10)
par(new=T)
scatterplot3d(data2$x,data2$y,data2$z,color=data2$group,pch="*")$points3d(c(mean(data2$x),1.55),
                                                                          c(mean(data2$y),0),
                                                                          c(mean(data2$z),9.86),
                                                                          col="yellow",type="l",pch=16,lwd=10)
par(new=T)
scatterplot3d(data2$x,data2$y,data2$z,color=data2$group,pch="*") 

####(4)
library("smacof")
scale_data=scale(data2[,1:3],scale=TRUE)
sim=cor(t(scale_data))
#to dissim#
dissim=sim2diss(sim)
mds=cmdscale(dissim,k=2,eig=T)
result_mds=as.data.frame(mds$points)
result_mds$group=data2$group
plot(result_mds[,1]~result_mds[,2],pch=16,col=result_mds$group)
legend("topleft",lty=1,col=c(unique(data2$group)),legend=c("group1","group2","group3")) 
diss2=sim2diss(data3)
mds2=cmdscale(data3,k=2,eig=T)
result_mds2=as.data.frame(mds2$points)
plot(result_mds2[,1]~result_mds2[,2],pch=16,col=result_mds$group)
legend("topleft",lty=1,col=c(unique(data2$group)),legend=c("group1","group2","group3")) 
head(mds$eig)
summary(princomp(data2[,1:3],cor=F),loadings=T)
new_data=data2
new_data[dim(data2)[1]+1,]=c(-100,100,100,1)
fix(new_data)
pr.out3=princomp(new_data[,1:3],cor=T)
score3=as.data.frame(pr.out3$scores)
plot(score3[,1]~score3[,2],pch=16,col=new_data$group,ylim=c(-10,10))
pr.out2=princomp(data2[,1:3],cor=F)
score2=as.data.frame(pr.out2$scores)
plot(score2[,1]~score2[,2],pch=16,col=data2$group)

summary(pr.out3,loadings=T)
dim(data2[data2$group==3,])
##############problem 3##################
fix(data4)
data=as.data.frame(data4)
names(data4)
plot(data[,1]~data[,2],col=data$ind+1,pch=16)
legend("topleft",inset=0.05,lty=1,col=c(1,2),legend=c("ind=0","ind=1"))
abline(4,-1,col="blue",lwd=4)
abline(3,-1,col="blue",lty=2,lwd=2)
abline(5,-1,col="blue",lty=2,lwd=2)
for(i in seq(1,90,length.out=10)){
  for(j in seq(1,90,length.out=10))
  {scatter3D(data[,1],data[,2],data[,3],col=data$ind+1,pch=16,phi=i,theta=j);
    Sys.sleep(0.1)}
}
scatterplot3d(data[,1],data[,2],data[,3],color=data$ind+1,pch=16)
#first second and third respectively#
sub1=data[data$ind==1,]
sub0=data[data$ind==0,]
dim(data)
par(mfrow=c(3,2))
for (i in 1:3){
  name=c("gene",i,"cha:dark")
  plot(sub1[,i]~seq(1:length(sub1[,i])),col=i,main=name)
  name=c("gene",i,"cha:light")
  plot(sub0[,i]~seq(1:length(sub0[,i])),col=i,main=name)
}
par(mfrow=c(1,3))
for(i in 1:3){
  name=c("gene",i)
  boxplot(data[,i]~data$ind,main=name)
}

# using euclidean distance as similarity distance#
sim1=dist(data[,-5001],method="euclidean",diag=F)
mds1=cmdscale(sim1,k=3,eig=T)
names(mds1)
point1=as.data.frame(mds1$points)
point1$i=data$ind
plot(point1[,1]~point1[,2],col=point1$i+1,pch=16)
scatterplot3d(point1[,1],point1[,2],point1[,3],color=data$ind+1,pch=16)
##use corrlation#
sim1=cor(t(data[,-5001]))
sim1=sim2diss(sim1)
mds1=cmdscale(sim1,k=3,eig=T)
names(mds1)
point1=as.data.frame(mds1$points)
point1$i=data$ind
plot(point1[,1]~point1[,2],col=point1$i+1,pch=16)
scatterplot3d(point1[,1],point1[,2],point1[,3],color=data$ind+1,pch=16)
#mahanlanobios#
sim=dist(data[,-5001])
sim=sim1
mds2=mds(sim,ndim=3,type="ordinal")
names(mds2)
mds2$stress
point1=as.data.frame(mds2$conf)
point1$i=data$ind
plot(point1[,1]~point1[,2],col=point1$i+1,pch=16)
scatterplot3d(point1[,1],point1[,2],point1[,3],color=data$ind+1,pch=16)
for(i in seq(1,90,length.out=10)){
  for(j in seq(1,90,length.out=10))
  {scatter3D(point1[,1],point1[,2],point1[,3],col=point1$i+1,pch=16,phi=i,theta=j);
    Sys.sleep(0.1)}
}
scatter3D(point1[,1],point1[,2],point1[,3],col=point1$i+1,pch=16,phi=30,theta=40)