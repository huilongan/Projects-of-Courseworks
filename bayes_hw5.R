data=cbind(c(89,84,81,87,79),
           c(88,77,87,92,81),
           c(97,92,87,89,80),
           c(94,79,85,84,88))
rownames(data)=seq(1,5)
colnames(data)=c("A","B","C","D")
com.dat=matrix(rep(NA),5,16)
rownames(com.dat)=seq(1,5)
colnames(com.dat)=rep(c("A","B","C","D"),4)
com.dat
for(i in 1:5){
  for(j in 1:16){
    com.dat[i,j]=ifelse(j==1|j==6|j==11|j==16,"yobs","ymis")
  }
}
com.dat
sum(com.dat=="yobs")
sum(com.dat=="ymis")
I=matrix(rep(NA),5,16)
rownames(I)=seq(1,5)
colnames(I)=rep(c("A","B","C","D"),4)
for(i in 1:5){
  for(j in 1:16){
    I[i,j]=ifelse(com.dat[i,j]=="yobs",1,0)
  }
}
I
X=rbind(rep(1,16),rep(2,16),rep(3,16),rep(4,16),rep(5,16))
rownames(X)=seq(1,5)
colnames(X)=rep(c("A","B","C","D"),4)
X
