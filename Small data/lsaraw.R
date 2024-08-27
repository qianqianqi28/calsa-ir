rm(list=ls())
library(Matrix)

### original data
X <- matrix(c(2,2,1,2,0,0,2,3,1,2,0,0,1,3,1,2,0,0,2,3,1,3,1,2,0,0,0,1,1,1,0,0,0,1,1,2),nrow = 6, ncol = 6)
rownames(X) <- c("1","2","3","4","5","6")
#rownames(X) <- c("doc1","doc2","doc3","doc4","doc5","doc6")
colnames(X) <- c("lion","tiger","cheetah","jaguar","porsche","ferrari")
X.count <- X
X.count

end <- rankMatrix(X.count)
end
#raw frequencies matrix D
X.svd <- svd(X.count)


X.svd$u <- X.svd$u[,1:end]
X.svd$d <- X.svd$d[1:end]
X.svd$v <- X.svd$v[,1:end]

round(X.svd$u,3)
round(X.svd$d,3)
round(X.svd$v,3)

options(scipen = 200)
alpha = -0.5
#The singular values, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F.
round(X.svd$d^alpha,3)
round(X.svd$d^(2*alpha),3)
round(X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha)),3)
Variance.qqq <- X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha))
Variance.qqq.firstdimensions <- Variance.qqq[1]+Variance.qqq[2]
Variance.qqq.firstdimensions



UD = X.svd$u%*%diag(X.svd$d^alpha)
VD <- X.svd$v%*%diag(X.svd$d^alpha)
# alpha = 1.5
# sum(X.svd$d^(2*alpha))
# sum(X.count^(2*alpha))
# sum(UD^(2*alpha))

#A two-dimensional plot of documents and terms for F.

x.min <- min(min(UD[,1]),min(VD[,1]))
y.min <- min(min(UD[,2]),min(VD[,2]))
x.max <- max(max(UD[,1]),max(VD[,1]))
y.max <- max(max(UD[,2]),max(VD[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1
col.set <- c("black","blue")
x.min
y.min
y.max

dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Small data\\figure\\lsarawalphaminus05.eps")
par(mar=c(5,6,4,1)+.1)

#plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, main=expression(paste(alpha, " = -0.5", " (LSA-RAW)")), xlim=c(-0.25,0),ylim=c(-0.45,0.2), xlab = "Dimension 1: 0.119 (1.7%)", ylab = "Dimension 2: 0.307 (4.5%)", col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, xlim=c(-0.25,0),ylim=c(-0.45,0.2), xlab = "Dimension 1: 0.119 (1.7%)", ylab = "Dimension 2: 0.307 (4.5%)", col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
text(UD[,1], UD[,2]-0.01, col = col.set[1], label = rownames(X.count),cex = 1.75)
lines(c(-0.35, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-1.2*max.total, 1.2*max.total), lwd=1.75, length=0.15)



k1 =-0.3
# k2 = floor(max.total)
k2 = floor(0)

k = 0.1 
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.03,i, cex = 1.55)
  
}
k1 =-0.6
k2 = 0.6
k = 0.1
for (i in round(seq(k1,k2,k),1)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.08*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.75 )
points(VD[,1], VD[,2], type = "p", pch = 1, cex = 1.8)
text(VD[,1]-0.015, VD[,2]+0.02, label = colnames(X.count), cex = 1.75)
dev.off()



alpha = 0
#The singular values, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F.
round(X.svd$d^alpha,3)
round(X.svd$d^(2*alpha),3)
round(X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha)),3)
Variance.qqq <- X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha))
Variance.qqq.firstdimensions <- Variance.qqq[1]+Variance.qqq[2]
Variance.qqq.firstdimensions



UD = X.svd$u%*%diag(X.svd$d^alpha)
VD <- X.svd$v%*%diag(X.svd$d^alpha)


#A two-dimensional plot of documents and terms for F.
x.min <- min(min(UD[,1]),min(VD[,1]))
y.min <- min(min(UD[,2]),min(VD[,2]))
x.max <- max(max(UD[,1]),max(VD[,1]))
y.max <- max(max(UD[,2]),max(VD[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1
col.set <- c("black","blue")
x.min
y.min
y.max
# plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, #xlim=c(x.min,0),ylim=c(y.min,y.max),
#      xlab = "Dimension 1: 70.985 (85.5%)", ylab = "Dimension 2: 10.635 (12.8%)",col.lab=col.set[1],cex.lab=1.25, lwd.ticks = 0.1)
# text(UD[,1], UD[,2], col = col.set[1], label = rownames(X.count),cex = 1.25)
# points(VD[,1], VD[,2], type = "p", pch = 1)
# text(VD[,1], VD[,2], label = colnames(X.count), cex = 1.25)

dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Small data\\figure\\lsarawalpha0.eps")
par(mar=c(5,6,4,1)+.1)

#plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, main=expression(paste(alpha, " = 0", " (LSA-RAW)")), xlim=c(-0.65,0),ylim=c(-0.8,0.5), xlab = "Dimension 1: 1 (20%)", ylab = "Dimension 2: 1 (20%)",col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, xlim=c(-0.65,0),ylim=c(-0.8,0.5), xlab = "Dimension 1: 1 (20%)", ylab = "Dimension 2: 1 (20%)",col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
text(UD[,1], UD[,2]-0.02, col = col.set[1], label = rownames(X.count),cex = 1.75)
lines(c(-0.85, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-0.9, 0.5), lwd=1.75, length=0.15)


k1 =-0.85
# k2 = floor(max.total)
k2 = floor(0)

k = 0.2 
for (i in round(seq(k1,k2,k),1)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.02,i, cex = 1.55)
  
}
k1 =-0.8
k2 = 0.45
k = 0.2
for (i in round(seq(k1,k2,k),1)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.035*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.75)
points(VD[,1], VD[,2], type = "p", pch = 1, cex = 1.8)
text(VD[,1], VD[,2]+0.02, label = colnames(X.count), cex = 1.75)
dev.off()


alpha = 0.5
#The singular values, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F.
round(X.svd$d^alpha,3)
round(X.svd$d^(2*alpha),3)
round(X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha)),3)
Variance.qqq <- X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha))
Variance.qqq.firstdimensions <- Variance.qqq[1]+Variance.qqq[2]
Variance.qqq.firstdimensions



UD = X.svd$u%*%diag(X.svd$d^alpha)
VD <- X.svd$v%*%diag(X.svd$d^alpha)


#A two-dimensional plot of documents and terms for F.
x.min <- min(min(UD[,1]),min(VD[,1]))
y.min <- min(min(UD[,2]),min(VD[,2]))
x.max <- max(max(UD[,1]),max(VD[,1]))
y.max <- max(max(UD[,2]),max(VD[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1
col.set <- c("black","blue")
x.min
y.min
y.max

dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Small data\\figure\\lsarawalpha05.eps")
par(mar=c(5,6,4,1)+.1)

#plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, main=expression(paste(alpha, " = 0.5", " (LSA-RAW)")), xlim=c(-2,0),ylim=c(-1.65,0.65), xlab = "Dimension 1: 8.425 (62.3%)", ylab = "Dimension 2: 3.261 (24.1%)",col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, xlim=c(-2,0),ylim=c(-1.65,0.65), xlab = "Dimension 1: 8.425 (62.3%)", ylab = "Dimension 2: 3.261 (24.1%)",col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
text(UD[,1], UD[,2]-0.05, col = col.set[1], label = rownames(X.count), cex = 1.75)
lines(c(-2.1, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-2, 8), lwd=1.75, length=0.15)


k1 =-2
# k2 = floor(max.total)
k2 = floor(0)

k = 0.4
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.01,i, cex = 1.55)
  
}
k1 =-1.6
k2 = 0.8
k = 0.4
for (i in round(seq(k1,k2,k),1)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.02*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.75)
points(VD[,1], VD[,2], type = "p", pch = 1, cex = 1.8)
text(VD[,1], VD[,2]+0.05, label = colnames(X.count), cex = 1.75)
dev.off()


alpha = 1
#The singular values, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F.
round(X.svd$d^alpha,3)
round(X.svd$d^(2*alpha),3)
round(X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha)),3)
Variance.qqq <- X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha))
Variance.qqq.firstdimensions <- Variance.qqq[1]+Variance.qqq[2]
Variance.qqq.firstdimensions



UD = X.svd$u%*%diag(X.svd$d^alpha)
VD <- X.svd$v%*%diag(X.svd$d^alpha)



#A two-dimensional plot of documents and terms for F.
x.min <- min(min(UD[,1]),min(X.svd$v[,1]))
y.min <- min(min(UD[,2]),min(X.svd$v[,2]))
x.max <- max(max(UD[,1]),max(X.svd$v[,1]))
y.max <- max(max(UD[,2]),max(X.svd$v[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1
col.set <- c("black","blue")
x.min
y.min
y.max
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Small data\\figure\\lsarawalpha1.eps")
par(mar=c(5,6,4,1)+.1)
#plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, main=expression(paste(alpha, " = 1", " (LSA-RAW)")), xlim=c(-6,0),ylim=c(-3,2.1), xlab = "Dimension 1: 70.985 (85.5%)", ylab = "Dimension 2: 10.635 (12.8%)",col.lab=col.set[1],axes=F,cex.lab=1.8, cex = 1.75)
plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, xlim=c(-6,0),ylim=c(-3,2.1), xlab = "Dimension 1: 70.985 (85.5%)", ylab = "Dimension 2: 10.635 (12.8%)",col.lab=col.set[1],axes=F,cex.lab=1.8, cex = 1.75)
text(UD[,1], UD[,2]-0.1, col = col.set[1], label = rownames(X.count), cex = 1.75)
lines(c(-1.2*max.total, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-1.2*max.total/2, 1.2*max.total/2), lwd=1.75, length=0.15)


k1 =ceiling(-1.2*max.total)
# k2 = floor(max.total)
k2 = floor(0)

k = 1 
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.02,i, cex = 1.55)
  
}
k1 =ceiling(-1.2*max.total/2)
k2 = floor(1.2*max.total/2)
k = 1
for (i in seq(k1,k2,k)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.02*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.75)
points(VD[,1], VD[,2], type = "p", pch = 1, cex = 1.8)
text(VD[,1], VD[,2]+0.1, label = colnames(X.count), cex = 1.75)
dev.off()


alpha = 1.5
#The singular values, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F.
round(X.svd$d^alpha,3)
round(X.svd$d^(2*alpha),3)
round(X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha)),3)
Variance.qqq <- X.svd$d^(2*alpha)/sum(X.svd$d^(2*alpha))
Variance.qqq.firstdimensions <- Variance.qqq[1]+Variance.qqq[2]
Variance.qqq.firstdimensions



UD = X.svd$u%*%diag(X.svd$d^alpha)
VD <- X.svd$v%*%diag(X.svd$d^alpha)


#A two-dimensional plot of documents and terms for F.
x.min <- min(min(UD[,1]),min(VD[,1]))
y.min <- min(min(UD[,2]),min(VD[,2]))
x.max <- max(max(UD[,1]),max(VD[,1]))
y.max <- max(max(UD[,2]),max(VD[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1
col.set <- c("black","blue")
x.min
y.min
y.max
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Small data\\figure\\lsarawalpha15.eps")
par(mar=c(5,6,4,1)+.1)

#plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, main=expression(paste(alpha, " = 1.5", " (LSA-RAW)")), xlim=c(-17, 0),ylim=c(-6,3), xlab = "Dimension 1: 598.063 (94.3%)", ylab = "Dimension 2: 34.684 (5.5%)",col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
plot(UD[,1], UD[,2],type="p",pch=20,col = col.set[1],asp = 1, xlim=c(-17, 0),ylim=c(-6,3), xlab = "Dimension 1: 598.063 (94.3%)", ylab = "Dimension 2: 34.684 (5.5%)",col.lab=col.set[1],axes=F,cex.lab=1.75, cex = 1.8)
text(UD[,1], UD[,2]-0.3, col = col.set[1], label = rownames(X.count), cex = 1.75)
lines(c(-17, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-6.2, 3), lwd=1.75, length=0.15)


k1 =-16
# k2 = floor(max.total)
k2 = floor(0)

k = 2
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.01,i, cex = 1.55)
  
}
k1 =-6
k2 = 2.5
k = 2
for (i in round(seq(k1,k2,k),1)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.015*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

text(-(xlim[2] - xlim[1])*0.01,-(ylim[2] - ylim[1])*0.07,"O", cex = 1.75)
points(VD[,1], VD[,2], type = "p", pch = 1, cex = 1.8)
text(VD[,1], VD[,2]+0.3, label = colnames(X.count), cex = 1.75)
dev.off()



