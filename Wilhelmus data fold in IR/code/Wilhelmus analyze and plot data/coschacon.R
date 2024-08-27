rm(list=ls())

#raw
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\raw\\raw")
load("meanprerawWH.Rdata")

#lsaraw
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\raw\\lsa\\p value")
load("meanprelsarawWH.Rdata")

#caraw
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\raw\\ca\\p value")
load("meanprecarawWH.Rdata")

#nrowl1
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl1\\nrowl1")
load("meanprenrowl1WH.Rdata")

#lsanrowL1
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl1\\lsa\\p value")
load("meanprelsanrowl1WH.Rdata")

#canrowL1
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl1\\ca\\p value")
load("meanprecanrowl1WH.Rdata")

#nrowl2
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl2\\nrowl2")
load("meanprenrowl2WH.Rdata")

#lsanrowL2
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl2\\lsa\\p value")
load("meanprelsanrowl2WH.Rdata")

#canrowL2
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl2\\ca\\p value")
load("meanprecanrowl2WH.Rdata")

#tfidf
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\tfidf\\tfidf")
load("meanpretfidfWH.Rdata")

#lsatfidf
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\tfidf\\lsa\\p value")
load("meanprelsatfidfWH.Rdata")


#catfidf
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\tfidf\\ca\\p value")
load("meanprecatfidfWH.Rdata")



# raw

raw_cos_WH_dim <-rep(raw_cos_WH, times=length(lsaraw_WH_dim))
raw_cos_WH_p <-rep(raw_cos_WH, times=length(lsaraw_WH_p))
round(raw_cos_WH,3)
#lsaraw
max_p_cos_lsaraw_WH <- matrix(NA, nrow = 1, ncol = length(lsaraw_WH_p))
which_max_p_cos_lsaraw_WH <- matrix(NA, nrow = 1, ncol = length(lsaraw_WH_p))
max_dim_cos_lsaraw_WH <- matrix(NA, nrow = 1, ncol = length(lsaraw_WH_dim))
which_max_dim_cos_lsaraw_WH <- matrix(NA, nrow = 1, ncol = length(lsaraw_WH_dim))

#lsaraw
for (i in 1:length(lsaraw_WH_p)) {
  max_p_cos_lsaraw_WH[1,i] <- max(lsaraw_cos_WH[i, ])
  which_max_p_cos_lsaraw_WH[1,i] <- lsaraw_WH_dim[which.max(lsaraw_cos_WH[i, ])]
}

round(max(max_p_cos_lsaraw_WH),3)
round(100*(max(max_p_cos_lsaraw_WH)-raw_cos_WH)/raw_cos_WH,1)

lsaraw_WH_p[which(lsaraw_cos_WH==max(lsaraw_cos_WH),arr.ind=T)[1]]
lsaraw_WH_dim[which(lsaraw_cos_WH==max(lsaraw_cos_WH),arr.ind=T)[2]]


#lsaraw
for (i in 1:length(lsaraw_WH_dim)) {
  max_dim_cos_lsaraw_WH[1,i] <- max(lsaraw_cos_WH[, i])
  which_max_dim_cos_lsaraw_WH[1,i] <- lsaraw_WH_p[which.max(lsaraw_cos_WH[, i])]
}

#caraw
max_p_cos_caraw_WH <- matrix(NA, nrow = 1, ncol = length(caraw_WH_p))
which_max_p_cos_caraw_WH <- matrix(NA, nrow = 1, ncol = length(caraw_WH_p))
max_dim_caraw_cos_WH <- matrix(NA, nrow = 1, ncol = length(caraw_WH_dim))
which_max_dim_cos_caraw_WH <- matrix(NA, nrow = 1, ncol = length(caraw_WH_dim))

#caraw
for (i in 1:length(caraw_WH_p)) {
  max_p_cos_caraw_WH[1,i] <- max(caraw_cos_WH[i, ])
  which_max_p_cos_caraw_WH[1,i] <- caraw_WH_dim[which.max(caraw_cos_WH[i, ])]
}

round(max(max_p_cos_caraw_WH),3)
round(100*(max(max_p_cos_caraw_WH)-raw_cos_WH)/raw_cos_WH,1)

caraw_WH_p[which(caraw_cos_WH==max(caraw_cos_WH),arr.ind=T)[1]]
caraw_WH_dim[which(caraw_cos_WH==max(caraw_cos_WH),arr.ind=T)[2]]



#caraw
for (i in 1:length(caraw_WH_dim)) {
  max_dim_caraw_cos_WH[1,i] <- max(caraw_cos_WH[, i])
  which_max_dim_cos_caraw_WH[1,i] <- caraw_WH_p[which.max(caraw_cos_WH[, i])]
}



# nrowl1
nrowl1_cos_WH_dim <-rep(nrowl1_cos_WH, times=length(lsanrowl1_WH_dim))
nrowl1_cos_WH_p <-rep(nrowl1_cos_WH, times=length(lsanrowl1_WH_p))
round(nrowl1_cos_WH,3)
#lsanrowl1
max_p_cos_lsanrowl1_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_WH_p))
which_max_p_cos_lsanrowl1_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_WH_p))
max_dim_cos_lsanrowl1_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_WH_dim))
which_max_dim_cos_lsanrowl1_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_WH_dim))

#lsanrowl1
for (i in 1:length(lsanrowl1_WH_p)) {
  max_p_cos_lsanrowl1_WH[1,i] <- max(lsanrowl1_cos_WH[i, ])
  which_max_p_cos_lsanrowl1_WH[1,i] <- lsanrowl1_WH_dim[which.max(lsanrowl1_cos_WH[i, ])]
}

round(max(max_p_cos_lsanrowl1_WH),3)
round(100*(max(max_p_cos_lsanrowl1_WH)-nrowl1_cos_WH)/nrowl1_cos_WH,1)

lsanrowl1_WH_p[which(lsanrowl1_cos_WH==max(lsanrowl1_cos_WH),arr.ind=T)[1]]
lsanrowl1_WH_dim[which(lsanrowl1_cos_WH==max(lsanrowl1_cos_WH),arr.ind=T)[2]]


#lsanrowl1
for (i in 1:length(lsanrowl1_WH_dim)) {
  max_dim_cos_lsanrowl1_WH[1,i] <- max(lsanrowl1_cos_WH[, i])
  which_max_dim_cos_lsanrowl1_WH[1,i] <- lsanrowl1_WH_p[which.max(lsanrowl1_cos_WH[, i])]
}



#canrowl1
max_p_cos_canrowl1_WH <- matrix(NA, nrow = 1, ncol = length(canrowl1_WH_p))
which_max_p_cos_canrowl1_WH <- matrix(NA, nrow = 1, ncol = length(canrowl1_WH_p))
max_dim_cos_canrowl1_WH <- matrix(NA, nrow = 1, ncol = length(canrowl1_WH_dim))
which_max_dim_cos_canrowl1_WH <- matrix(NA, nrow = 1, ncol = length(canrowl1_WH_dim))

#canrowl1
for (i in 1:length(canrowl1_WH_p)) {
  max_p_cos_canrowl1_WH[1,i] <- max(canrowl1_cos_WH[i, ])
  which_max_p_cos_canrowl1_WH[1,i] <- canrowl1_WH_dim[which.max(canrowl1_cos_WH[i, ])]
}

round(max(max_p_cos_canrowl1_WH),3)
round(100*(max(max_p_cos_canrowl1_WH)-nrowl1_cos_WH)/nrowl1_cos_WH,1)

canrowl1_WH_p[which(canrowl1_cos_WH==max(canrowl1_cos_WH),arr.ind=T)[1]]
canrowl1_WH_dim[which(canrowl1_cos_WH==max(canrowl1_cos_WH),arr.ind=T)[2]]

#canrowl1
for (i in 1:length(canrowl1_WH_dim)) {
  max_dim_cos_canrowl1_WH[1,i] <- max(canrowl1_cos_WH[, i])
  which_max_dim_cos_canrowl1_WH[1,i] <- canrowl1_WH_p[which.max(canrowl1_cos_WH[, i])]
}


# nrowl2
nrowl2_cos_WH_dim <-rep(nrowl2_cos_WH, times=length(lsanrowl2_WH_dim))
nrowl2_cos_WH_p <-rep(nrowl2_cos_WH, times=length(lsanrowl2_WH_p))
round(nrowl2_cos_WH,3)
#lsanrowl2
max_p_cos_lsanrowl2_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_WH_p))
which_max_p_cos_lsanrowl2_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_WH_p))
max_dim_cos_lsanrowl2_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_WH_dim))
which_max_dim_cos_lsanrowl2_WH <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_WH_dim))

#lsanrowl2
for (i in 1:length(lsanrowl2_WH_p)) {
  max_p_cos_lsanrowl2_WH[1,i] <- max(lsanrowl2_cos_WH[i, ])
  which_max_p_cos_lsanrowl2_WH[1,i] <- lsanrowl2_WH_dim[which.max(lsanrowl2_cos_WH[i, ])]
}

round(max(max_p_cos_lsanrowl2_WH),3)
round(100*(max(max_p_cos_lsanrowl2_WH)-nrowl2_cos_WH)/nrowl2_cos_WH,1)

lsanrowl2_WH_p[which(lsanrowl2_cos_WH==max(lsanrowl2_cos_WH),arr.ind=T)[1]]
lsanrowl2_WH_dim[which(lsanrowl2_cos_WH==max(lsanrowl2_cos_WH),arr.ind=T)[2]]


#lsanrowl2
for (i in 1:length(lsanrowl2_WH_dim)) {
  max_dim_cos_lsanrowl2_WH[1,i] <- max(lsanrowl2_cos_WH[, i])
  which_max_dim_cos_lsanrowl2_WH[1,i] <- lsanrowl2_WH_p[which.max(lsanrowl2_cos_WH[, i])]
}



#canrowl2
max_p_cos_canrowl2_WH <- matrix(NA, nrow = 1, ncol = length(canrowl2_WH_p))
which_max_p_cos_canrowl2_WH <- matrix(NA, nrow = 1, ncol = length(canrowl2_WH_p))
max_dim_cos_canrowl2_WH <- matrix(NA, nrow = 1, ncol = length(canrowl2_WH_dim))
which_max_dim_cos_canrowl2_WH <- matrix(NA, nrow = 1, ncol = length(canrowl2_WH_dim))

#canrowl2
for (i in 1:length(canrowl2_WH_p)) {
  max_p_cos_canrowl2_WH[1,i] <- max(canrowl2_cos_WH[i, ])
  which_max_p_cos_canrowl2_WH[1,i] <- canrowl2_WH_dim[which.max(canrowl2_cos_WH[i, ])]
}

round(max(max_p_cos_canrowl2_WH),3)
round(100*(max(max_p_cos_canrowl2_WH)-nrowl2_cos_WH)/nrowl2_cos_WH,1)

canrowl2_WH_p[which(canrowl2_cos_WH==max(canrowl2_cos_WH),arr.ind=T)[1]]
canrowl2_WH_dim[which(canrowl2_cos_WH==max(canrowl2_cos_WH),arr.ind=T)[2]]

#canrowl2
for (i in 1:length(canrowl2_WH_dim)) {
  max_dim_cos_canrowl2_WH[1,i] <- max(canrowl2_cos_WH[, i])
  which_max_dim_cos_canrowl2_WH[1,i] <- canrowl2_WH_p[which.max(canrowl2_cos_WH[, i])]
}


# tfidf
tfidf_cos_WH_dim <-rep(tfidf_cos_WH, times=length(lsatfidf_WH_dim))
tfidf_cos_WH_p <-rep(tfidf_cos_WH, times=length(lsatfidf_WH_p))
round(tfidf_cos_WH,3)
#lsatfidf
max_p_cos_lsatfidf_WH <- matrix(NA, nrow = 1, ncol = length(lsatfidf_WH_p))
which_max_p_cos_lsatfidf_WH <- matrix(NA, nrow = 1, ncol = length(lsatfidf_WH_p))
max_dim_cos_lsatfidf_WH <- matrix(NA, nrow = 1, ncol = length(lsatfidf_WH_dim))
which_max_dim_cos_lsatfidf_WH <- matrix(NA, nrow = 1, ncol = length(lsatfidf_WH_dim))

#lsatfidf
for (i in 1:length(lsatfidf_WH_p)) {
  max_p_cos_lsatfidf_WH[1,i] <- max(lsatfidf_cos_WH[i, ])
  which_max_p_cos_lsatfidf_WH[1,i] <- lsatfidf_WH_dim[which.max(lsatfidf_cos_WH[i, ])]
}

round(max(max_p_cos_lsatfidf_WH),3)
round(100*(max(max_p_cos_lsatfidf_WH)-tfidf_cos_WH)/tfidf_cos_WH,1)

lsatfidf_WH_p[which(lsatfidf_cos_WH==max(lsatfidf_cos_WH),arr.ind=T)[1]]
lsatfidf_WH_dim[which(lsatfidf_cos_WH==max(lsatfidf_cos_WH),arr.ind=T)[2]]

#lsatfidf
for (i in 1:length(lsatfidf_WH_dim)) {
  max_dim_cos_lsatfidf_WH[1,i] <- max(lsatfidf_cos_WH[, i])
  which_max_dim_cos_lsatfidf_WH[1,i] <- lsatfidf_WH_p[which.max(lsatfidf_cos_WH[, i])]
}



#catfidf
max_p_cos_catfidf_WH <- matrix(NA, nrow = 1, ncol = length(catfidf_WH_p))
which_max_p_cos_catfidf_WH <- matrix(NA, nrow = 1, ncol = length(catfidf_WH_p))
max_dim_cos_catfidf_WH <- matrix(NA, nrow = 1, ncol = length(catfidf_WH_dim))
which_max_dim_cos_catfidf_WH <- matrix(NA, nrow = 1, ncol = length(catfidf_WH_dim))

#catfidf
for (i in 1:length(catfidf_WH_p)) {
  max_p_cos_catfidf_WH[1,i] <- max(catfidf_cos_WH[i, ])
  which_max_p_cos_catfidf_WH[1,i] <- catfidf_WH_dim[which.max(catfidf_cos_WH[i, ])]
}

round(max(max_p_cos_catfidf_WH),3)
round(100*(max(max_p_cos_catfidf_WH)-tfidf_cos_WH)/tfidf_cos_WH,1)


catfidf_WH_p[which(catfidf_cos_WH==max(catfidf_cos_WH),arr.ind=T)[1]]
catfidf_WH_dim[which(catfidf_cos_WH==max(catfidf_cos_WH),arr.ind=T)[2]]


#catfidf
for (i in 1:length(catfidf_WH_dim)) {
  max_dim_cos_catfidf_WH[1,i] <- max(catfidf_cos_WH[, i])
  which_max_dim_cos_catfidf_WH[1,i] <- catfidf_WH_p[which.max(catfidf_cos_WH[, i])]
}


#standard p
max(lsaraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(lsanrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(lsanrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(lsatfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])

max(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(canrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(canrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(catfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])

lsaraw_WH_dim[which.max(lsaraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
lsaraw_WH_dim[which.max(lsanrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
lsaraw_WH_dim[which.max(lsanrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
lsaraw_WH_dim[which.max(lsatfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]

which(lsaraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(lsaraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))
which(lsanrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(lsanrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))
which(lsanrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(lsanrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))
which(lsatfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(lsatfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))


caraw_WH_dim[which.max(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
caraw_WH_dim[which.max(canrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
caraw_WH_dim[which.max(canrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
caraw_WH_dim[which.max(catfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]

which(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))


length(lsaraw_WH_dim)
set_dim = 1:length(lsaraw_WH_dim)
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\figure\\coschacon\\Fcosstandardpwilhelmus.eps")
par(mar=c(5,6,4,1)+.1)

plot(lsaraw_WH_dim[set_dim],raw_cos_WH_dim[set_dim],type="p",lty=1, pch = 15, xlab = "k", ylab = "MAP (cosine)", xaxt = "n", cex.lab=1.25,#xlim = c(1, 20),
     ylim = c(0.24, 0.69))#

lines(lsaraw_WH_dim[set_dim],raw_cos_WH_dim[set_dim],col=1,lty=1, pch = 15, cex=1.25)

points(lsaraw_WH_dim[set_dim],lsaraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_dim[set_dim],lsanrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_dim[set_dim],lsanrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_dim[set_dim],lsatfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=2, pch = 16)

lines(lsaraw_WH_dim[set_dim],lsaraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_dim[set_dim],lsanrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_dim[set_dim],lsanrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_dim[set_dim],lsatfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=2, pch = 16)


points(caraw_WH_dim[set_dim],caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17)

lines(caraw_WH_dim[set_dim],caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17)
axis(side = 1, at = lsaraw_WH_dim[set_dim],  labels = FALSE)
axis(side = 1, at = c(1, seq(10,100,10)),  labels = c(1, seq(10,100,10)))

#title(expression(paste(alpha, " = 1 (wilhelmus)")),lwd=3)
legend("bottomright",ncol=3,cex=1,c("RAW","LSA-RAW", "LSA-NROWL1", "LSA-NROWL2", "LSA-TFIDF", "CA"),col=c(1,1,2,3,4,1), lty = c(1,2,2,2,2,3), pch = c(15,16,16,16,16,17))
#,inset=c(0,0.4)
dev.off()


#constant dim
dim_con <- c(4, 6, 7, 9, 12, 22)
lsaraw_WH_p[which.max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[1]) < 0.01)])]
lsaraw_WH_p[which.max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[2]) < 0.01)])]
lsaraw_WH_p[which.max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[3]) < 0.01)])]
lsaraw_WH_p[which.max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[4]) < 0.01)])]
lsaraw_WH_p[which.max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[5]) < 0.01)])]
lsaraw_WH_p[which.max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[6]) < 0.01)])]


which(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[1]) < 0.01)]== max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[1]) < 0.01)]))
which(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[2]) < 0.01)]== max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[2]) < 0.01)]))
which(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[3]) < 0.01)]== max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[3]) < 0.01)]))
which(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[4]) < 0.01)]== max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[4]) < 0.01)]))
which(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[5]) < 0.01)]== max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[5]) < 0.01)]))
which(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[6]) < 0.01)]== max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[6]) < 0.01)]))


round(max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[1]) < 0.01)]),3)
round(max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[2]) < 0.01)]),3)
round(max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[3]) < 0.01)]),3)
round(max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[4]) < 0.01)]),3)
round(max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[5]) < 0.01)]),3)
round(max(lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[6]) < 0.01)]),3)



round(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[1]) < 0.01)], 3)
round(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[2]) < 0.01)], 3)
round(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[3]) < 0.01)], 3)
round(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[4]) < 0.01)], 3)
round(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[5]) < 0.01)], 3)
round(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[6]) < 0.01)], 3)


setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\figure\\coschacon\\Fcosstandarddwilhelmus.eps")
par(mar=c(5,6,4,1)+.1)


#plot(lsaraw_WH_p,raw_cos_WH_p,type="p",lty=1, pch = 15, xlab = expression(paste(alpha)), ylab = "MAP (coslidean)", xaxt = "n", cex.lab=1.25,#xlim = c(1, 20),
#ylim = c(0.2, 0.83))#

#lines(lsaraw_WH_p,raw_cos_WH_p,col=1,lty=1, pch = 15, cex=1.25)

plot(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=2, pch = 16, xlab = expression(paste(alpha)), ylab = "MAP (cosine)", xaxt = "n", cex.lab=1.25,# xlim = c(1, 20),
     ylim = c(0.1, 0.68))
points(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=2, pch = 16)
points(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=2, pch = 16)


lines(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=2, pch = 16)
lines(lsaraw_WH_p,lsaraw_cos_WH[,which(abs(lsaraw_WH_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=2, pch = 16)


points(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[1]) < 0.01)], times=length(caraw_WH_p)),col=1, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[2]) < 0.01)], times=length(caraw_WH_p)),col=2, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[3]) < 0.01)], times=length(caraw_WH_p)),col=3, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[4]) < 0.01)], times=length(caraw_WH_p)),col=4, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[5]) < 0.01)], times=length(caraw_WH_p)),col=5, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[6]) < 0.01)], times=length(caraw_WH_p)),col=6, cex=1.25, lty=3, pch = 17)


lines(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[1]) < 0.01)], times=length(caraw_WH_p)),col=1, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[2]) < 0.01)], times=length(caraw_WH_p)),col=2, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[3]) < 0.01)], times=length(caraw_WH_p)),col=3, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[4]) < 0.01)], times=length(caraw_WH_p)),col=4, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[5]) < 0.01)], times=length(caraw_WH_p)),col=5, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,rep(caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),which(abs(caraw_WH_dim - dim_con[6]) < 0.01)], times=length(caraw_WH_p)),col=6, cex=1.25, lty=3, pch = 17)

axis(side = 1, at = lsaraw_WH_p,  labels = FALSE)
axis(side = 1, at = seq(-6, 8, 1),  labels = seq(-6, 8, 1))


#title(expression(paste("k = 10 (wilhelmus)")),lwd=3)
legend("bottomleft",cex=1,ncol=2,c("LSA-RAW (k = 4)", "LSA-RAW (k = 6)", "LSA-RAW (k = 7)", "LSA-RAW (k = 9)", "LSA-RAW (k = 12)", "LSA-RAW (k = 22)", "CA (k = 4)", "CA (k = 6)", "CA (k = 7)", "CA (k = 9)", "CA (k = 12)", "CA (k = 22)"),col=c(1,2,3,4,5,6,1,2,3,4,5,6), lty = c(2,2,2,2,2,2,3,3,3,3,3,3), pch = c(16,16,16,16,16,16,17,17,17,17,17,17))
#,inset=c(0,0.4)
dev.off()
#c(4, 6, 7, 12, 14, 15, 41, 90)

max(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(canrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(canrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])
max(catfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])


caraw_WH_dim[which.max(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
caraw_WH_dim[which.max(canrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
caraw_WH_dim[which.max(canrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]
caraw_WH_dim[which.max(catfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),])]

which(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(caraw_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))

which(canrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(canrowl1_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))

which(canrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(canrowl2_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))

which(catfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),] == max(catfidf_cos_WH[which(abs(lsaraw_WH_p - 1) < 0.01),]))



set_dim = 1:length(lsaraw_WH_dim)
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\figure\\coschacon\\Fcosstandardpwilhelmuscaweisch.eps")
par(mar=c(5,6,4,1)+.1)


plot(caraw_WH_dim[set_dim],caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17, xlab = "k", ylab = "MAP (cosine)", xaxt = "n", cex.lab=1.25,#xlim = c(1, 20),
     ylim = c(0.3, 0.69))
points(caraw_WH_dim[set_dim],canrowl1_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=3, pch = 17)
points(caraw_WH_dim[set_dim],canrowl2_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=3, pch = 17)
points(caraw_WH_dim[set_dim],catfidf_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=3, pch = 17)

lines(caraw_WH_dim[set_dim],caraw_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_dim[set_dim],canrowl1_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_dim[set_dim],canrowl2_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_dim[set_dim],catfidf_cos_WH[which(abs(caraw_WH_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=3, pch = 17)
axis(side = 1, at = lsaraw_WH_dim[set_dim],  labels = FALSE)
axis(side = 1, at = c(1, seq(10,100,10)),  labels = c(1, seq(10,100,10)))

#title(expression(paste(alpha, " = 1 (wilhelmus)")),lwd=3)
legend("bottomright",ncol=1,cex=1,c("CA-RAW", "CA-NROWL1", "CA-NROWL2", "CA-TFIDF"),col=c(1,2,3,4), lty = c(3,3,3,3), pch = c(17,17,17,17))
#,inset=c(0,0.4)
dev.off()


#constant dim
dim_con <- c(4, 6, 7, 9, 12, 22)
lsaraw_WH_p[which.max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[1]) < 0.01)])]
lsaraw_WH_p[which.max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[2]) < 0.01)])]
lsaraw_WH_p[which.max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[3]) < 0.01)])]
lsaraw_WH_p[which.max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[4]) < 0.01)])]
lsaraw_WH_p[which.max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[5]) < 0.01)])]
lsaraw_WH_p[which.max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[6]) < 0.01)])]

which(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[1]) < 0.01)] == max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[1]) < 0.01)]))
which(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[2]) < 0.01)] == max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[2]) < 0.01)]))
which(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[3]) < 0.01)] == max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[3]) < 0.01)]))
which(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[4]) < 0.01)] == max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[4]) < 0.01)]))
which(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[5]) < 0.01)] == max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[5]) < 0.01)]))
which(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[6]) < 0.01)] == max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[6]) < 0.01)]))


round(max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[1]) < 0.01)]),3)
round(max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[2]) < 0.01)]),3)
round(max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[3]) < 0.01)]),3)
round(max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[4]) < 0.01)]),3)
round(max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[5]) < 0.01)]),3)
round(max(caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[6]) < 0.01)]),3)


setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\figure\\coschacon\\Fcosstandarddwilhelmuscaweiexp.eps")
par(mar=c(5,6,4,1)+.1)

plot(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=3, pch = 17, xlab = expression(paste(alpha)), ylab = "MAP (cosine)", xaxt = "n", cex.lab=1.25,# xlim = c(1, 20),
     ylim = c(0.2, 0.69))

points(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=3, pch = 17)
points(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=3, pch = 17)

lines(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=3, pch = 17)
lines(caraw_WH_p,caraw_cos_WH[,which(abs(caraw_WH_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=3, pch = 17)

axis(side = 1, at = lsaraw_WH_p,  labels = FALSE)
axis(side = 1, at = seq(-6, 8, 1),  labels = seq(-6, 8, 1))


#title(expression(paste("k = 10 (wilhelmus)")),lwd=3)
legend("bottom",cex=1,ncol=1,c("CA-RAW (k = 4)", "CA-RAW (k = 6)", "CA-RAW (k = 7)", "CA-RAW (k = 9)", "CA-RAW (k = 12)", "CA-RAW (k = 22)"),col=c(1,2,3,4,5,6), lty = c(3,3,3,3,3,3), pch = c(17,17,17,17,17,17))
#,inset=c(0,0.4)
dev.off()


# Drang <- 1:length(lsaraw_WH_dim)
# 
# setEPS()
# postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\figure\\coschacon\\Fcosoptimalalphawilhelmus.eps")
# par(mar=c(5,6,4,1)+.1)
# 
# plot(lsaraw_WH_dim[Drang],which_max_dim_cos_lsaraw_WH[1, Drang], type="b", lty=2, pch = 16, xlab = "k", ylab = expression(paste("The optimal ", alpha, " (cosine)")), cex.lab=1.25, ylim = c(-6, 8), xaxt = "n", yaxt = "n")
# points(lsaraw_WH_dim[Drang],which_max_dim_cos_lsanrowl1_WH[1,Drang],col=2, lty=2, pch = 16, cex=1.25)
# points(lsaraw_WH_dim[Drang],which_max_dim_cos_lsanrowl2_WH[1,Drang],col=3, lty=2, pch = 16, cex=1.25)
# points(lsaraw_WH_dim[Drang],which_max_dim_cos_lsatfidf_WH[1,Drang],col=4, lty=2, pch = 16, cex=1.25)
# 
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_lsaraw_WH[1,Drang],col=1, lty=2, pch = 16, cex=1.25)
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_lsanrowl1_WH[1,Drang],col=2, lty=2, pch = 16, cex=1.25)
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_lsanrowl2_WH[1,Drang],col=3, lty=2, pch = 16, cex=1.25)
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_lsatfidf_WH[1,Drang],col=4, lty=2, pch = 16, cex=1.25)
# 
# points(caraw_WH_dim[Drang],which_max_dim_cos_caraw_WH[1,Drang],col=1, lty=3, pch = 17, cex=1.25)
# points(lsaraw_WH_dim[Drang],which_max_dim_cos_canrowl1_WH[1,Drang],col=2, lty=3, pch = 17, cex=1.25)
# points(lsaraw_WH_dim[Drang],which_max_dim_cos_canrowl2_WH[1,Drang],col=3, lty=3, pch = 17, cex=1.25)
# points(lsaraw_WH_dim[Drang],which_max_dim_cos_catfidf_WH[1,Drang],col=4, lty=3, pch = 17, cex=1.25)
# 
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_caraw_WH[1,Drang],col=1, lty=3, pch = 17, cex=1.25)
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_canrowl1_WH[1,Drang],col=2, lty=3, pch = 17, cex=1.25)
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_canrowl2_WH[1,Drang],col=3, lty=3, pch = 17, cex=1.25)
# lines(lsaraw_WH_dim[Drang],which_max_dim_cos_catfidf_WH[1,Drang],col=4, lty=3, pch = 17, cex=1.25)
# 
# title(expression(paste("The optimal ", alpha, " (Wilhelmus)")),lwd=3)
# legend("bottomright",ncol=2,cex=1,c("LSA-RAW", "LSA-NROWL1", "LSA-NROWL2", "LSA-TFIDF", "CA-RAW", "CA-NROWL1", "CA-NROWL2", "CA-TFIDF"),col=c(1,2,3,4,1,2,3,4), lty = c(2,2,2,2,3,3,3,3), pch = c(16,16,16,16,17,17,17,17))
# axis(side = 2, at = seq(-6, 8, 1), labels=seq(-6, 8, 1))#, las = 3)#
# axis(side = 1, at = lsaraw_WH_dim,  labels = FALSE)
# axis(side = 1, at = c(1, seq(10,100,10)),  labels = c(1, seq(10,100,10)))
# dev.off()
# 

#lsa

for (i in 1:length(lsaraw_WH_p)) {
  print(paste0(i, ": ", which(lsaraw_cos_WH[i, ] == max(lsaraw_cos_WH[i, ]))))
}

for (i in 1:length(lsaraw_WH_p)) {
  print(paste0(i, ": ", which(lsanrowl1_cos_WH[i, ] == max(lsanrowl1_cos_WH[i, ]))))
}

for (i in 1:length(lsaraw_WH_p)) {
  print(paste0(i, ": ", which(lsanrowl2_cos_WH[i, ] == max(lsanrowl2_cos_WH[i, ]))))
}

# multiple
for (i in 1:length(lsaraw_WH_p)) {
  print(paste0(i, ": ", which(lsatfidf_cos_WH[i, ] == max(lsatfidf_cos_WH[i, ]))))
}

#ca
# multiple
for (i in 1:length(caraw_WH_p)) {
  print(paste0(i, ": ", which(caraw_cos_WH[i, ] == max(caraw_cos_WH[i, ]))))
}

for (i in 1:length(caraw_WH_p)) {
  print(paste0(i, ": ", which(canrowl1_cos_WH[i, ] == max(canrowl1_cos_WH[i, ]))))
}
# multiple
for (i in 1:length(caraw_WH_p)) {
  print(paste0(i, ": ", which(canrowl2_cos_WH[i, ] == max(canrowl2_cos_WH[i, ]))))
}

for (i in 1:length(caraw_WH_p)) {
  print(paste0(i, ": ", which(catfidf_cos_WH[i, ] == max(catfidf_cos_WH[i, ]))))
}



lsaraw_WH_p[which.max(max_p_cos_lsaraw_WH[1,])]
lsaraw_WH_p[which.max(max_p_cos_lsanrowl1_WH[1,])]
lsaraw_WH_p[which.max(max_p_cos_lsanrowl2_WH[1,])]
lsaraw_WH_p[which.max(max_p_cos_lsatfidf_WH[1,])]

which(max_p_cos_lsaraw_WH[1,] == max(max_p_cos_lsaraw_WH[1,]))
which(max_p_cos_lsanrowl1_WH[1,] == max(max_p_cos_lsanrowl1_WH[1,]))
which(max_p_cos_lsanrowl2_WH[1,] == max(max_p_cos_lsanrowl2_WH[1,]))
which(max_p_cos_lsatfidf_WH[1,] == max(max_p_cos_lsatfidf_WH[1,]))

sum(lsaraw_cos_WH == max(lsaraw_cos_WH))
sum(lsanrowl1_cos_WH == max(lsanrowl1_cos_WH))
sum(lsanrowl2_cos_WH == max(lsanrowl2_cos_WH))
sum(lsatfidf_cos_WH == max(lsatfidf_cos_WH))

lsaraw_WH_p[which(lsaraw_cos_WH == max(lsaraw_cos_WH), arr.ind = TRUE)[1]]
lsaraw_WH_p[which(lsanrowl1_cos_WH == max(lsanrowl1_cos_WH), arr.ind = TRUE)[1]]
lsaraw_WH_p[which(lsanrowl2_cos_WH == max(lsanrowl2_cos_WH), arr.ind = TRUE)[1]]
lsaraw_WH_p[which(lsatfidf_cos_WH == max(lsatfidf_cos_WH), arr.ind = TRUE)[1]]

lsaraw_WH_dim[which(lsaraw_cos_WH == max(lsaraw_cos_WH), arr.ind = TRUE)[2]]
lsaraw_WH_dim[which(lsanrowl1_cos_WH == max(lsanrowl1_cos_WH), arr.ind = TRUE)[2]]
lsaraw_WH_dim[which(lsanrowl2_cos_WH == max(lsanrowl2_cos_WH), arr.ind = TRUE)[2]]
lsaraw_WH_dim[which(lsatfidf_cos_WH == max(lsatfidf_cos_WH), arr.ind = TRUE)[2]]

caraw_WH_p[which.max(max_p_cos_caraw_WH[1,])]
caraw_WH_p[which.max(max_p_cos_canrowl1_WH[1,])]
caraw_WH_p[which.max(max_p_cos_canrowl2_WH[1,])]
caraw_WH_p[which.max(max_p_cos_catfidf_WH[1,])]

which(max_p_cos_caraw_WH[1,] == max(max_p_cos_caraw_WH[1,]))
which(max_p_cos_canrowl1_WH[1,] == max(max_p_cos_canrowl1_WH[1,]))
which(max_p_cos_canrowl2_WH[1,] == max(max_p_cos_canrowl2_WH[1,]))
which(max_p_cos_catfidf_WH[1,] == max(max_p_cos_catfidf_WH[1,]))

sum(caraw_cos_WH == max(caraw_cos_WH))
sum(canrowl1_cos_WH == max(canrowl1_cos_WH))
sum(canrowl2_cos_WH == max(canrowl2_cos_WH))
sum(catfidf_cos_WH == max(catfidf_cos_WH))

lsaraw_WH_p[which(caraw_cos_WH == max(caraw_cos_WH), arr.ind = TRUE)[1]]
lsaraw_WH_p[which(canrowl1_cos_WH == max(canrowl1_cos_WH), arr.ind = TRUE)[1]]
lsaraw_WH_p[which(canrowl2_cos_WH == max(canrowl2_cos_WH), arr.ind = TRUE)[1]]
lsaraw_WH_p[which(catfidf_cos_WH == max(catfidf_cos_WH), arr.ind = TRUE)[1]]

lsaraw_WH_dim[which(caraw_cos_WH == max(caraw_cos_WH), arr.ind = TRUE)[2]]
lsaraw_WH_dim[which(canrowl1_cos_WH == max(canrowl1_cos_WH), arr.ind = TRUE)[2]]
lsaraw_WH_dim[which(canrowl2_cos_WH == max(canrowl2_cos_WH), arr.ind = TRUE)[2]]
lsaraw_WH_dim[which(catfidf_cos_WH == max(catfidf_cos_WH), arr.ind = TRUE)[2]]


plotrange <- 1:length(caraw_WH_p)

setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\figure\\coschacon\\Fcosoptimalmapwilhelmus.eps")
par(mar=c(5,6,4,1)+.1)

plot(caraw_WH_p[plotrange],raw_cos_WH_p, ylim = c(0.2, 0.69),type="b", lty=1, pch = 15, xlab = expression(paste(alpha), " value"), ylab = "MAP (cosine)", cex.lab=1.25, xaxt = "n")
points(caraw_WH_p[plotrange],nrowl1_cos_WH_p,col=2,lty=1, pch = 15, cex=1.25)
points(caraw_WH_p[plotrange],nrowl2_cos_WH_p,col=3,lty=1, pch = 15, cex=1.25)
points(caraw_WH_p[plotrange],tfidf_cos_WH_p,col=4,lty=1, pch = 15, cex=1.25)

lines(caraw_WH_p[plotrange],raw_cos_WH_p,col=1,lty=1, pch = 15, cex=1.25)
lines(caraw_WH_p[plotrange],nrowl1_cos_WH_p,col=2,lty=1, pch = 15, cex=1.25)
lines(caraw_WH_p[plotrange],nrowl2_cos_WH_p,col=3,lty=1, pch = 15, cex=1.25)
lines(caraw_WH_p[plotrange],tfidf_cos_WH_p,col=4,lty=1, pch = 15, cex=1.25)


points(caraw_WH_p[plotrange],max_p_cos_lsaraw_WH[1,plotrange],col=1, cex=1.25,lty=2, pch = 16)
points(caraw_WH_p[plotrange],max_p_cos_lsanrowl1_WH[1,plotrange],col=2, cex=1.25,lty=2, pch = 16)
points(caraw_WH_p[plotrange],max_p_cos_lsanrowl2_WH[1,plotrange],col=3, cex=1.25,lty=2, pch = 16)
points(caraw_WH_p[plotrange],max_p_cos_lsatfidf_WH[1,plotrange],col=4, cex=1.25,lty=2, pch = 16)

lines(caraw_WH_p[plotrange],max_p_cos_lsaraw_WH[1,plotrange],col=1, cex=1.25,lty=2, pch = 16)
lines(caraw_WH_p[plotrange],max_p_cos_lsanrowl1_WH[1,plotrange],col=2, cex=1.25,lty=2, pch = 16)
lines(caraw_WH_p[plotrange],max_p_cos_lsanrowl2_WH[1,plotrange],col=3, cex=1.25,lty=2, pch = 16)
lines(caraw_WH_p[plotrange],max_p_cos_lsatfidf_WH[1,plotrange],col=4, cex=1.25,lty=2, pch = 16)



points(caraw_WH_p[plotrange],max_p_cos_caraw_WH[1,plotrange],col=1, cex=1.25,lty=3, pch = 17)
points(caraw_WH_p[plotrange],max_p_cos_canrowl1_WH[1,plotrange],col=2, cex=1.25,lty=3, pch = 17)
points(caraw_WH_p[plotrange],max_p_cos_canrowl2_WH[1,plotrange],col=3, cex=1.25,lty=3, pch = 17)
points(caraw_WH_p[plotrange],max_p_cos_catfidf_WH[1,plotrange],col=4, cex=1.25,lty=3, pch = 17)

lines(caraw_WH_p[plotrange],max_p_cos_caraw_WH[1,plotrange],col=1, cex=1.25,lty=3, pch = 17)
lines(caraw_WH_p[plotrange],max_p_cos_canrowl1_WH[1,plotrange],col=2, cex=1.25,lty=3, pch = 17)
lines(caraw_WH_p[plotrange],max_p_cos_canrowl2_WH[1,plotrange],col=3, cex=1.25,lty=3, pch = 17)
lines(caraw_WH_p[plotrange],max_p_cos_catfidf_WH[1,plotrange],col=4, cex=1.25,lty=3, pch = 17)

# points(caraw_WH_p[which.max(max_lsaraw_cos_WH)], max(max_lsaraw_cos_WH), col = 1, type = "p", pch = 20)
# points(caraw_WH_p[which.max(max_lsanrowl1_cos_WH)], max(max_lsanrowl1_cos_WH), col = 2, type = "p", pch = 20)
# points(caraw_WH_p[which.max(max_lsanrowl2_cos_WH)], max(max_lsanrowl2_cos_WH), col = 3, type = "p", pch = 20)
# points(caraw_WH_p[which.max(max_lsatfidf_cos_WH)], max(max_lsatfidf_cos_WH), col = 4, type = "p", pch = 20)
# points(caraw_WH_p[which.max(max_p_caraw_cos_WH)], max(max_p_caraw_cos_WH), col = 5, type = "p", pch = 20)
# 
#title(expression(paste("Optimal k (Wilhelmus)")),lwd=3)
legend("bottomleft", ncol=3,cex=1,c("RAW", "NROWL1", "NROWL2", "TFIDF","LSA-RAW", "LSA-NROWL1", "LSA-NROWL2", "LSA-TFIDF", "CA-RAW", "CA-NROWL1", "CA-NROWL2", "CA-TFIDF"),col=c(1,2,3,4,1,2,3,4,1,2,3,4), lty = c(1,1,1,1,2,2,2,2,3,3,3,3), pch = c(15,15,15,15,16,16,16,16,17,17,17,17))
#,inset=c(0,0.4)
axis(side = 1, at = lsaraw_WH_p,  labels = FALSE)
axis(side = 1, at = seq(-6, 8, 1),  labels = seq(-6, 8, 1))
dev.off()

#legend("topright",inset=c(0,0.1),cex=1,c("RAW", "NROWL1", "NROWL2", "TFIDF","LSARAW - k = 6", "LSANROWL1 - k = 5", "LSANROWL2 - k = 5", "LSATFIDF - k = 6", "CA - k = 8"),col=c(1,2,3,4,1,2,3,4,5), lty = c(2,2,2,2,1,1,1,1,1))

lsaraw_WH_p[which.max(max_p_cos_lsaraw_WH[1,])]
lsaraw_WH_p[which.max(max_p_cos_lsanrowl1_WH[1,])]
lsaraw_WH_p[which.max(max_p_cos_lsanrowl2_WH[1,])]
lsaraw_WH_p[which.max(max_p_cos_lsatfidf_WH[1,])]

caraw_WH_p[which.max(max_p_cos_caraw_WH[1,])]
caraw_WH_p[which.max(max_p_cos_canrowl1_WH[1,])]
caraw_WH_p[which.max(max_p_cos_canrowl2_WH[1,])]
caraw_WH_p[which.max(max_p_cos_catfidf_WH[1,])]


