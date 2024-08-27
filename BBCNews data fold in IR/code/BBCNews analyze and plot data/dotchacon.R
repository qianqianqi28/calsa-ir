rm(list=ls())

#raw
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\raw\\raw")
load("meanprerawBN.Rdata")

#lsaraw
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\raw\\lsa\\p value")
load("meanprelsarawBN.Rdata")

#caraw
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\raw\\ca\\p value")
load("meanprecarawBN.Rdata")

#nrowl1
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl1\\nrowl1")
load("meanprenrowl1BN.Rdata")

#lsanrowL1
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl1\\lsa\\p value")
load("meanprelsanrowl1BN.Rdata")

#canrowL1
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl1\\ca\\p value")
load("meanprecanrowl1BN.Rdata")

#nrowl2
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl2\\nrowl2")
load("meanprenrowl2BN.Rdata")

#lsanrowL2
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl2\\lsa\\p value")
load("meanprelsanrowl2BN.Rdata")

#canrowL2
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl2\\ca\\p value")
load("meanprecanrowl2BN.Rdata")

#tfidf
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\tfidf\\tfidf")
load("meanpretfidfBN.Rdata")

#lsatfidf
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\tfidf\\lsa\\p value")
load("meanprelsatfidfBN.Rdata")


#catfidf
setwd("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\tfidf\\ca\\p value")
load("meanprecatfidfBN.Rdata")



del_dim <- c(seq(21, 49, by=2), seq(51, 59, by=2))

lsaraw_BN_dim <- lsaraw_BN_dim[-del_dim]
lsanrowl1_BN_dim <- lsanrowl1_BN_dim[-del_dim]
lsanrowl2_BN_dim <- lsanrowl2_BN_dim[-del_dim]
lsatfidf_BN_dim <- lsatfidf_BN_dim[-del_dim]

caraw_BN_dim <- caraw_BN_dim[-del_dim]
canrowl1_BN_dim <- canrowl1_BN_dim[-del_dim]
canrowl2_BN_dim <- canrowl2_BN_dim[-del_dim]
catfidf_BN_dim <- catfidf_BN_dim[-del_dim]

lsaraw_euc_BN <- lsaraw_euc_BN[, -del_dim]
lsanrowl1_euc_BN <- lsanrowl1_euc_BN[, -del_dim]
lsanrowl2_euc_BN <- lsanrowl2_euc_BN[, -del_dim]
lsatfidf_euc_BN <- lsatfidf_euc_BN[, -del_dim]

caraw_euc_BN <- caraw_euc_BN[, -del_dim]
canrowl1_euc_BN <- canrowl1_euc_BN[, -del_dim]
canrowl2_euc_BN <- canrowl2_euc_BN[, -del_dim]
catfidf_euc_BN <- catfidf_euc_BN[, -del_dim]

lsaraw_dot_BN <- lsaraw_dot_BN[, -del_dim]
lsanrowl1_dot_BN <- lsanrowl1_dot_BN[, -del_dim]
lsanrowl2_dot_BN <- lsanrowl2_dot_BN[, -del_dim]
lsatfidf_dot_BN <- lsatfidf_dot_BN[, -del_dim]

caraw_dot_BN <- caraw_dot_BN[, -del_dim]
canrowl1_dot_BN <- canrowl1_dot_BN[, -del_dim]
canrowl2_dot_BN <- canrowl2_dot_BN[, -del_dim]
catfidf_dot_BN <- catfidf_dot_BN[, -del_dim]

lsaraw_cos_BN <- lsaraw_cos_BN[, -del_dim]
lsanrowl1_cos_BN <- lsanrowl1_cos_BN[, -del_dim]
lsanrowl2_cos_BN <- lsanrowl2_cos_BN[, -del_dim]
lsatfidf_cos_BN <- lsatfidf_cos_BN[, -del_dim]

caraw_cos_BN <- caraw_cos_BN[, -del_dim]
canrowl1_cos_BN <- canrowl1_cos_BN[, -del_dim]
canrowl2_cos_BN <- canrowl2_cos_BN[, -del_dim]
catfidf_cos_BN <- catfidf_cos_BN[, -del_dim]



# raw

raw_dot_BN_dim <-rep(raw_dot_BN, times=length(lsaraw_BN_dim))
raw_dot_BN_p <-rep(raw_dot_BN, times=length(lsaraw_BN_p))
round(raw_dot_BN,3)
#lsaraw
max_p_dot_lsaraw_BN <- matrix(NA, nrow = 1, ncol = length(lsaraw_BN_p))
which_max_p_dot_lsaraw_BN <- matrix(NA, nrow = 1, ncol = length(lsaraw_BN_p))
max_dim_dot_lsaraw_BN <- matrix(NA, nrow = 1, ncol = length(lsaraw_BN_dim))
which_max_dim_dot_lsaraw_BN <- matrix(NA, nrow = 1, ncol = length(lsaraw_BN_dim))

#lsaraw
for (i in 1:length(lsaraw_BN_p)) {
  max_p_dot_lsaraw_BN[1,i] <- max(lsaraw_dot_BN[i, ])
  which_max_p_dot_lsaraw_BN[1,i] <- lsaraw_BN_dim[which.max(lsaraw_dot_BN[i, ])]
}

round(max(max_p_dot_lsaraw_BN),3)
round(100*(max(max_p_dot_lsaraw_BN)-raw_dot_BN)/raw_dot_BN,1)

lsaraw_BN_p[which(lsaraw_dot_BN==max(lsaraw_dot_BN),arr.ind=T)[1]]
lsaraw_BN_dim[which(lsaraw_dot_BN==max(lsaraw_dot_BN),arr.ind=T)[2]]


#lsaraw
for (i in 1:length(lsaraw_BN_dim)) {
  max_dim_dot_lsaraw_BN[1,i] <- max(lsaraw_dot_BN[, i])
  which_max_dim_dot_lsaraw_BN[1,i] <- lsaraw_BN_p[which.max(lsaraw_dot_BN[, i])]
}

#caraw
max_p_dot_caraw_BN <- matrix(NA, nrow = 1, ncol = length(caraw_BN_p))
which_max_p_dot_caraw_BN <- matrix(NA, nrow = 1, ncol = length(caraw_BN_p))
max_dim_caraw_dot_BN <- matrix(NA, nrow = 1, ncol = length(caraw_BN_dim))
which_max_dim_dot_caraw_BN <- matrix(NA, nrow = 1, ncol = length(caraw_BN_dim))

#caraw
for (i in 1:length(caraw_BN_p)) {
  max_p_dot_caraw_BN[1,i] <- max(caraw_dot_BN[i, ])
  which_max_p_dot_caraw_BN[1,i] <- caraw_BN_dim[which.max(caraw_dot_BN[i, ])]
}

round(max(max_p_dot_caraw_BN),3)
round(100*(max(max_p_dot_caraw_BN)-raw_dot_BN)/raw_dot_BN,1)

caraw_BN_p[which(caraw_dot_BN==max(caraw_dot_BN),arr.ind=T)[1]]
caraw_BN_dim[which(caraw_dot_BN==max(caraw_dot_BN),arr.ind=T)[2]]



#caraw
for (i in 1:length(caraw_BN_dim)) {
  max_dim_caraw_dot_BN[1,i] <- max(caraw_dot_BN[, i])
  which_max_dim_dot_caraw_BN[1,i] <- caraw_BN_p[which.max(caraw_dot_BN[, i])]
}



# nrowl1
nrowl1_dot_BN_dim <-rep(nrowl1_dot_BN, times=length(lsanrowl1_BN_dim))
nrowl1_dot_BN_p <-rep(nrowl1_dot_BN, times=length(lsanrowl1_BN_p))
round(nrowl1_dot_BN,3)
#lsanrowl1
max_p_dot_lsanrowl1_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_BN_p))
which_max_p_dot_lsanrowl1_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_BN_p))
max_dim_dot_lsanrowl1_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_BN_dim))
which_max_dim_dot_lsanrowl1_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl1_BN_dim))

#lsanrowl1
for (i in 1:length(lsanrowl1_BN_p)) {
  max_p_dot_lsanrowl1_BN[1,i] <- max(lsanrowl1_dot_BN[i, ])
  which_max_p_dot_lsanrowl1_BN[1,i] <- lsanrowl1_BN_dim[which.max(lsanrowl1_dot_BN[i, ])]
}

round(max(max_p_dot_lsanrowl1_BN),3)
round(100*(max(max_p_dot_lsanrowl1_BN)-nrowl1_dot_BN)/nrowl1_dot_BN,1)

lsanrowl1_BN_p[which(lsanrowl1_dot_BN==max(lsanrowl1_dot_BN),arr.ind=T)[1]]
lsanrowl1_BN_dim[which(lsanrowl1_dot_BN==max(lsanrowl1_dot_BN),arr.ind=T)[2]]


#lsanrowl1
for (i in 1:length(lsanrowl1_BN_dim)) {
  max_dim_dot_lsanrowl1_BN[1,i] <- max(lsanrowl1_dot_BN[, i])
  which_max_dim_dot_lsanrowl1_BN[1,i] <- lsanrowl1_BN_p[which.max(lsanrowl1_dot_BN[, i])]
}



#canrowl1
max_p_dot_canrowl1_BN <- matrix(NA, nrow = 1, ncol = length(canrowl1_BN_p))
which_max_p_dot_canrowl1_BN <- matrix(NA, nrow = 1, ncol = length(canrowl1_BN_p))
max_dim_dot_canrowl1_BN <- matrix(NA, nrow = 1, ncol = length(canrowl1_BN_dim))
which_max_dim_dot_canrowl1_BN <- matrix(NA, nrow = 1, ncol = length(canrowl1_BN_dim))

#canrowl1
for (i in 1:length(canrowl1_BN_p)) {
  max_p_dot_canrowl1_BN[1,i] <- max(canrowl1_dot_BN[i, ])
  which_max_p_dot_canrowl1_BN[1,i] <- canrowl1_BN_dim[which.max(canrowl1_dot_BN[i, ])]
}

round(max(max_p_dot_canrowl1_BN),3)
round(100*(max(max_p_dot_canrowl1_BN)-nrowl1_dot_BN)/nrowl1_dot_BN,1)

canrowl1_BN_p[which(canrowl1_dot_BN==max(canrowl1_dot_BN),arr.ind=T)[1]]
canrowl1_BN_dim[which(canrowl1_dot_BN==max(canrowl1_dot_BN),arr.ind=T)[2]]

#canrowl1
for (i in 1:length(canrowl1_BN_dim)) {
  max_dim_dot_canrowl1_BN[1,i] <- max(canrowl1_dot_BN[, i])
  which_max_dim_dot_canrowl1_BN[1,i] <- canrowl1_BN_p[which.max(canrowl1_dot_BN[, i])]
}


# nrowl2
nrowl2_dot_BN_dim <-rep(nrowl2_dot_BN, times=length(lsanrowl2_BN_dim))
nrowl2_dot_BN_p <-rep(nrowl2_dot_BN, times=length(lsanrowl2_BN_p))
round(nrowl2_dot_BN,3)
#lsanrowl2
max_p_dot_lsanrowl2_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_BN_p))
which_max_p_dot_lsanrowl2_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_BN_p))
max_dim_dot_lsanrowl2_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_BN_dim))
which_max_dim_dot_lsanrowl2_BN <- matrix(NA, nrow = 1, ncol = length(lsanrowl2_BN_dim))

#lsanrowl2
for (i in 1:length(lsanrowl2_BN_p)) {
  max_p_dot_lsanrowl2_BN[1,i] <- max(lsanrowl2_dot_BN[i, ])
  which_max_p_dot_lsanrowl2_BN[1,i] <- lsanrowl2_BN_dim[which.max(lsanrowl2_dot_BN[i, ])]
}

round(max(max_p_dot_lsanrowl2_BN),3)
round(100*(max(max_p_dot_lsanrowl2_BN)-nrowl2_dot_BN)/nrowl2_dot_BN,1)

lsanrowl2_BN_p[which(lsanrowl2_dot_BN==max(lsanrowl2_dot_BN),arr.ind=T)[1]]
lsanrowl2_BN_dim[which(lsanrowl2_dot_BN==max(lsanrowl2_dot_BN),arr.ind=T)[2]]


#lsanrowl2
for (i in 1:length(lsanrowl2_BN_dim)) {
  max_dim_dot_lsanrowl2_BN[1,i] <- max(lsanrowl2_dot_BN[, i])
  which_max_dim_dot_lsanrowl2_BN[1,i] <- lsanrowl2_BN_p[which.max(lsanrowl2_dot_BN[, i])]
}



#canrowl2
max_p_dot_canrowl2_BN <- matrix(NA, nrow = 1, ncol = length(canrowl2_BN_p))
which_max_p_dot_canrowl2_BN <- matrix(NA, nrow = 1, ncol = length(canrowl2_BN_p))
max_dim_dot_canrowl2_BN <- matrix(NA, nrow = 1, ncol = length(canrowl2_BN_dim))
which_max_dim_dot_canrowl2_BN <- matrix(NA, nrow = 1, ncol = length(canrowl2_BN_dim))

#canrowl2
for (i in 1:length(canrowl2_BN_p)) {
  max_p_dot_canrowl2_BN[1,i] <- max(canrowl2_dot_BN[i, ])
  which_max_p_dot_canrowl2_BN[1,i] <- canrowl2_BN_dim[which.max(canrowl2_dot_BN[i, ])]
}

round(max(max_p_dot_canrowl2_BN),3)
round(100*(max(max_p_dot_canrowl2_BN)-nrowl2_dot_BN)/nrowl2_dot_BN,1)

canrowl2_BN_p[which(canrowl2_dot_BN==max(canrowl2_dot_BN),arr.ind=T)[1]]
canrowl2_BN_dim[which(canrowl2_dot_BN==max(canrowl2_dot_BN),arr.ind=T)[2]]

#canrowl2
for (i in 1:length(canrowl2_BN_dim)) {
  max_dim_dot_canrowl2_BN[1,i] <- max(canrowl2_dot_BN[, i])
  which_max_dim_dot_canrowl2_BN[1,i] <- canrowl2_BN_p[which.max(canrowl2_dot_BN[, i])]
}


# tfidf
tfidf_dot_BN_dim <-rep(tfidf_dot_BN, times=length(lsatfidf_BN_dim))
tfidf_dot_BN_p <-rep(tfidf_dot_BN, times=length(lsatfidf_BN_p))
round(tfidf_dot_BN,3)
#lsatfidf
max_p_dot_lsatfidf_BN <- matrix(NA, nrow = 1, ncol = length(lsatfidf_BN_p))
which_max_p_dot_lsatfidf_BN <- matrix(NA, nrow = 1, ncol = length(lsatfidf_BN_p))
max_dim_dot_lsatfidf_BN <- matrix(NA, nrow = 1, ncol = length(lsatfidf_BN_dim))
which_max_dim_dot_lsatfidf_BN <- matrix(NA, nrow = 1, ncol = length(lsatfidf_BN_dim))

#lsatfidf
for (i in 1:length(lsatfidf_BN_p)) {
  max_p_dot_lsatfidf_BN[1,i] <- max(lsatfidf_dot_BN[i, ])
  which_max_p_dot_lsatfidf_BN[1,i] <- lsatfidf_BN_dim[which.max(lsatfidf_dot_BN[i, ])]
}

round(max(max_p_dot_lsatfidf_BN),3)
round(100*(max(max_p_dot_lsatfidf_BN)-tfidf_dot_BN)/tfidf_dot_BN,1)

lsatfidf_BN_p[which(lsatfidf_dot_BN==max(lsatfidf_dot_BN),arr.ind=T)[1]]
lsatfidf_BN_dim[which(lsatfidf_dot_BN==max(lsatfidf_dot_BN),arr.ind=T)[2]]

#lsatfidf
for (i in 1:length(lsatfidf_BN_dim)) {
  max_dim_dot_lsatfidf_BN[1,i] <- max(lsatfidf_dot_BN[, i])
  which_max_dim_dot_lsatfidf_BN[1,i] <- lsatfidf_BN_p[which.max(lsatfidf_dot_BN[, i])]
}



#catfidf
max_p_dot_catfidf_BN <- matrix(NA, nrow = 1, ncol = length(catfidf_BN_p))
which_max_p_dot_catfidf_BN <- matrix(NA, nrow = 1, ncol = length(catfidf_BN_p))
max_dim_dot_catfidf_BN <- matrix(NA, nrow = 1, ncol = length(catfidf_BN_dim))
which_max_dim_dot_catfidf_BN <- matrix(NA, nrow = 1, ncol = length(catfidf_BN_dim))

#catfidf
for (i in 1:length(catfidf_BN_p)) {
  max_p_dot_catfidf_BN[1,i] <- max(catfidf_dot_BN[i, ])
  which_max_p_dot_catfidf_BN[1,i] <- catfidf_BN_dim[which.max(catfidf_dot_BN[i, ])]
}

round(max(max_p_dot_catfidf_BN),3)
round(100*(max(max_p_dot_catfidf_BN)-tfidf_dot_BN)/tfidf_dot_BN,1)


catfidf_BN_p[which(catfidf_dot_BN==max(catfidf_dot_BN),arr.ind=T)[1]]
catfidf_BN_dim[which(catfidf_dot_BN==max(catfidf_dot_BN),arr.ind=T)[2]]


#catfidf
for (i in 1:length(catfidf_BN_dim)) {
  max_dim_dot_catfidf_BN[1,i] <- max(catfidf_dot_BN[, i])
  which_max_dim_dot_catfidf_BN[1,i] <- catfidf_BN_p[which.max(catfidf_dot_BN[, i])]
}


#standard p
max(lsaraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(lsanrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(lsanrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(lsatfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])

max(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(canrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(canrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(catfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])

lsaraw_BN_dim[which.max(lsaraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
lsaraw_BN_dim[which.max(lsanrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
lsaraw_BN_dim[which.max(lsanrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
lsaraw_BN_dim[which.max(lsatfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]

which(lsaraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(lsaraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))
which(lsanrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(lsanrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))
which(lsanrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(lsanrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))
which(lsatfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(lsatfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))

caraw_BN_dim[which.max(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
caraw_BN_dim[which.max(canrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
caraw_BN_dim[which.max(canrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
caraw_BN_dim[which.max(catfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]

which(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))

length(lsaraw_BN_dim)
set_dim = 1:length(lsaraw_BN_dim)
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\figure\\dotchacon\\Fdotstandardpbbcnews.eps")
par(mar=c(5,6,4,1)+.1)

plot(lsaraw_BN_dim[set_dim],raw_dot_BN_dim[set_dim],type="p",lty=1, pch = 15, xlab = "k", ylab = "MAP (dot)", xaxt = "n", cex.lab=1.25,#xlim = c(1, 20),
     ylim = c(0.2, 0.9))#

lines(lsaraw_BN_dim[set_dim],raw_dot_BN_dim[set_dim],col=1,lty=1, pch = 15, cex=1.25)

points(lsaraw_BN_dim[set_dim],lsaraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_dim[set_dim],lsanrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_dim[set_dim],lsanrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_dim[set_dim],lsatfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=2, pch = 16)

lines(lsaraw_BN_dim[set_dim],lsaraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_dim[set_dim],lsanrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_dim[set_dim],lsanrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_dim[set_dim],lsatfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=2, pch = 16)


points(caraw_BN_dim[set_dim],caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17)

lines(caraw_BN_dim[set_dim],caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17)
axis(side = 1, at = lsaraw_BN_dim[set_dim],  labels = FALSE)
axis(side = 1, at = c(1, seq(10,100,10)),  labels = c(1, seq(10,100,10)))

#title(expression(paste(alpha, " = 1 (BBCNews)")),lwd=3)
legend("bottomright",ncol=3,cex=1,c("RAW","LSA-RAW", "LSA-NROWL1", "LSA-NROWL2", "LSA-TFIDF", "CA"),col=c(1,1,2,3,4,1), lty = c(1,2,2,2,2,3), pch = c(15,16,16,16,16,17))
#,inset=c(0,0.4)
dev.off()



#constant dim
dim_con <- c(4, 6, 7, 12, 14, 15, 36, 90)
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[1]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[2]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[3]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[4]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[5]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[6]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[7]) < 0.01)])]
lsaraw_BN_p[which.max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[8]) < 0.01)])]


which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[1]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[1]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[2]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[2]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[3]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[3]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[4]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[4]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[5]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[5]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[6]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[6]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[7]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[7]) < 0.01)]))
which(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[8]) < 0.01)]== max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[8]) < 0.01)]))



round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[1]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[2]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[3]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[4]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[5]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[6]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[7]) < 0.01)]),3)
round(max(lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[8]) < 0.01)]),3)



round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[1]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[2]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[3]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[4]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[5]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[6]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[7]) < 0.01)], 3)
round(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[8]) < 0.01)], 3)


setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\figure\\dotchacon\\Fdotstandarddbbcnews.eps")
par(mar=c(5,6,4,1)+.1)


#plot(lsaraw_BN_p,raw_dot_BN_p,type="p",lty=1, pch = 15, xlab = expression(paste(alpha)), ylab = "MAP (dotlidean)", xaxt = "n", cex.lab=1.25,#xlim = c(1, 20),
#ylim = c(0.2, 0.83))#

#lines(lsaraw_BN_p,raw_dot_BN_p,col=1,lty=1, pch = 15, cex=1.25)

plot(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=2, pch = 16, xlab = expression(paste(alpha)), ylab = "MAP (dot)", xaxt = "n", cex.lab=1.25,# xlim = c(1, 20),
     ylim = c(0, 0.9))
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[7]) < 0.01)],col=7, cex=1.25, lty=2, pch = 16)
points(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[8]) < 0.01)],col=8, cex=1.25, lty=2, pch = 16)


lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[7]) < 0.01)],col=7, cex=1.25, lty=2, pch = 16)
lines(lsaraw_BN_p,lsaraw_dot_BN[,which(abs(lsaraw_BN_dim - dim_con[8]) < 0.01)],col=8, cex=1.25, lty=2, pch = 16)


points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[1]) < 0.01)], times=length(caraw_BN_p)),col=1, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[2]) < 0.01)], times=length(caraw_BN_p)),col=2, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[3]) < 0.01)], times=length(caraw_BN_p)),col=3, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[4]) < 0.01)], times=length(caraw_BN_p)),col=4, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[5]) < 0.01)], times=length(caraw_BN_p)),col=5, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[6]) < 0.01)], times=length(caraw_BN_p)),col=6, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[7]) < 0.01)], times=length(caraw_BN_p)),col=7, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[8]) < 0.01)], times=length(caraw_BN_p)),col=8, cex=1.25, lty=3, pch = 17)


lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[1]) < 0.01)], times=length(caraw_BN_p)),col=1, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[2]) < 0.01)], times=length(caraw_BN_p)),col=2, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[3]) < 0.01)], times=length(caraw_BN_p)),col=3, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[4]) < 0.01)], times=length(caraw_BN_p)),col=4, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[5]) < 0.01)], times=length(caraw_BN_p)),col=5, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[6]) < 0.01)], times=length(caraw_BN_p)),col=6, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[7]) < 0.01)], times=length(caraw_BN_p)),col=7, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,rep(caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),which(abs(caraw_BN_dim - dim_con[8]) < 0.01)], times=length(caraw_BN_p)),col=8, cex=1.25, lty=3, pch = 17)

axis(side = 1, at = lsaraw_BN_p,  labels = FALSE)
axis(side = 1, at = seq(-6, 8, 1),  labels = seq(-6, 8, 1))


#title(expression(paste("k = 10 (BBCNews)")),lwd=3)
legend("bottomleft",cex=1,ncol=2,c("LSA-RAW (k = 4)", "LSA-RAW (k = 6)", "LSA-RAW (k = 7)", "LSA-RAW (k = 12)", "LSA-RAW (k = 14)", "LSA-RAW (k = 15)","LSA-RAW (k = 36)","LSA-RAW (k = 90)", "CA (k = 4)", "CA (k = 6)", "CA (k = 7)", "CA (k = 12)", "CA (k = 14)", "CA (k = 15)","CA (k = 36)","CA (k = 90)"),col=c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8), lty = c(2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3), pch = c(16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17))
#,inset=c(0,0.4)
dev.off()
#c(4, 6, 7, 12, 14, 15, 41, 90)


max(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(canrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(canrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])
max(catfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])


caraw_BN_dim[which.max(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
caraw_BN_dim[which.max(canrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
caraw_BN_dim[which.max(canrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]
caraw_BN_dim[which.max(catfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),])]

which(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(caraw_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))

which(canrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(canrowl1_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))

which(canrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(canrowl2_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))

which(catfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),] == max(catfidf_dot_BN[which(abs(lsaraw_BN_p - 1) < 0.01),]))


set_dim = 1:length(lsaraw_BN_dim)
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\figure\\dotchacon\\Fdotstandardpbbcnewscaweisch.eps")
par(mar=c(5,6,4,1)+.1)


plot(caraw_BN_dim[set_dim],caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17, xlab = "k", ylab = "MAP (dot)", xaxt = "n", cex.lab=1.25,#xlim = c(1, 20),
     ylim = c(0.5, 0.9))
points(caraw_BN_dim[set_dim],canrowl1_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=3, pch = 17)
points(caraw_BN_dim[set_dim],canrowl2_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=3, pch = 17)
points(caraw_BN_dim[set_dim],catfidf_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=3, pch = 17)

lines(caraw_BN_dim[set_dim],caraw_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=1, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_dim[set_dim],canrowl1_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=2, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_dim[set_dim],canrowl2_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=3, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_dim[set_dim],catfidf_dot_BN[which(abs(caraw_BN_p - 1) < 0.01),set_dim],col=4, cex=1.25, lty=3, pch = 17)
axis(side = 1, at = lsaraw_BN_dim[set_dim],  labels = FALSE)
axis(side = 1, at = c(1, seq(10,100,10)),  labels = c(1, seq(10,100,10)))

#title(expression(paste(alpha, " = 1 (BBCNews)")),lwd=3)
legend("bottomright",ncol=1,cex=1,c("CA-RAW", "CA-NROWL1", "CA-NROWL2", "CA-TFIDF"),col=c(1,2,3,4), lty = c(3,3,3,3), pch = c(17,17,17,17))
#,inset=c(0,0.4)
dev.off()



#constant dim
dim_con <- c(4, 6, 7, 12, 14, 15, 36, 90)
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[1]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[2]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[3]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[4]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[5]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[6]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[7]) < 0.01)])]
lsaraw_BN_p[which.max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[8]) < 0.01)])]

which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[1]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[1]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[2]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[2]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[3]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[3]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[4]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[4]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[5]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[5]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[6]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[6]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[7]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[7]) < 0.01)]))
which(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[8]) < 0.01)] == max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[8]) < 0.01)]))


round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[1]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[2]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[3]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[4]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[5]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[6]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[7]) < 0.01)]),3)
round(max(caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[8]) < 0.01)]),3)


setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\figure\\dotchacon\\Fdotstandarddbbcnewscaweiexp.eps")
par(mar=c(5,6,4,1)+.1)

plot(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=3, pch = 17, xlab = expression(paste(alpha)), ylab = "MAP (dot)", xaxt = "n", cex.lab=1.25,# xlim = c(1, 20),
     ylim = c(0.3, 0.9))

points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[7]) < 0.01)],col=7, cex=1.25, lty=3, pch = 17)
points(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[8]) < 0.01)],col=8, cex=1.25, lty=3, pch = 17)

lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[1]) < 0.01)],col=1, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[2]) < 0.01)],col=2, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[3]) < 0.01)],col=3, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[4]) < 0.01)],col=4, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[5]) < 0.01)],col=5, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[6]) < 0.01)],col=6, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[7]) < 0.01)],col=7, cex=1.25, lty=3, pch = 17)
lines(caraw_BN_p,caraw_dot_BN[,which(abs(caraw_BN_dim - dim_con[8]) < 0.01)],col=8, cex=1.25, lty=3, pch = 17)

axis(side = 1, at = lsaraw_BN_p,  labels = FALSE)
axis(side = 1, at = seq(-6, 8, 1),  labels = seq(-6, 8, 1))


#title(expression(paste("k = 10 (BBCNews)")),lwd=3)
legend("bottomright",cex=1,ncol=1,c("CA-RAW (k = 4)", "CA-RAW (k = 6)", "CA-RAW (k = 7)", "CA-RAW (k = 12)", "CA-RAW (k = 14)", "CA-RAW (k = 15)", "CA-RAW (k = 36)", "CA-RAW (k = 90)"),col=c(1,2,3,4,5,6,7,8), lty = c(3,3,3,3,3,3,3,3), pch = c(17,17,17,17,17,17,17,17))
#,inset=c(0,0.4)
dev.off()


# Drang <- 1:length(lsaraw_BN_dim)
# 
# setEPS()
# postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\figure\\dotchacon\\Fdotoptimalalphabbcnews.eps")
# par(mar=c(5,6,4,1)+.1)
# 
# plot(lsaraw_BN_dim[Drang],which_max_dim_dot_lsaraw_BN[1, Drang], type="b", lty=2, pch = 16, xlab = "k", ylab = expression(paste("The optimal ", alpha, " (dot)")), cex.lab=1.25, ylim = c(-6, 8), xaxt = "n", yaxt = "n")
# points(lsaraw_BN_dim[Drang],which_max_dim_dot_lsanrowl1_BN[1,Drang],col=2, lty=2, pch = 16, cex=1.25)
# points(lsaraw_BN_dim[Drang],which_max_dim_dot_lsanrowl2_BN[1,Drang],col=3, lty=2, pch = 16, cex=1.25)
# points(lsaraw_BN_dim[Drang],which_max_dim_dot_lsatfidf_BN[1,Drang],col=4, lty=2, pch = 16, cex=1.25)
# 
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_lsaraw_BN[1,Drang],col=1, lty=2, pch = 16, cex=1.25)
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_lsanrowl1_BN[1,Drang],col=2, lty=2, pch = 16, cex=1.25)
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_lsanrowl2_BN[1,Drang],col=3, lty=2, pch = 16, cex=1.25)
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_lsatfidf_BN[1,Drang],col=4, lty=2, pch = 16, cex=1.25)
# 
# points(caraw_BN_dim[Drang],which_max_dim_dot_caraw_BN[1,Drang],col=1, lty=3, pch = 17, cex=1.25)
# points(lsaraw_BN_dim[Drang],which_max_dim_dot_canrowl1_BN[1,Drang],col=2, lty=3, pch = 17, cex=1.25)
# points(lsaraw_BN_dim[Drang],which_max_dim_dot_canrowl2_BN[1,Drang],col=3, lty=3, pch = 17, cex=1.25)
# points(lsaraw_BN_dim[Drang],which_max_dim_dot_catfidf_BN[1,Drang],col=4, lty=3, pch = 17, cex=1.25)
# 
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_caraw_BN[1,Drang],col=1, lty=3, pch = 17, cex=1.25)
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_canrowl1_BN[1,Drang],col=2, lty=3, pch = 17, cex=1.25)
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_canrowl2_BN[1,Drang],col=3, lty=3, pch = 17, cex=1.25)
# lines(lsaraw_BN_dim[Drang],which_max_dim_dot_catfidf_BN[1,Drang],col=4, lty=3, pch = 17, cex=1.25)
# 
# title(expression(paste("The optimal ", alpha, " (BBCNews)")),lwd=3)
# legend("bottomright",ncol=2,cex=1,c("LSA-RAW", "LSA-NROWL1", "LSA-NROWL2", "LSA-TFIDF", "CA-RAW", "CA-NROWL1", "CA-NROWL2", "CA-TFIDF"),col=c(1,2,3,4,1,2,3,4), lty = c(2,2,2,2,3,3,3,3), pch = c(16,16,16,16,17,17,17,17))
# axis(side = 2, at = seq(-6, 8, 1), labels=seq(-6, 8, 1))#, las = 3)#
# axis(side = 1, at = lsaraw_BN_dim,  labels = FALSE)
# axis(side = 1, at = c(1, seq(10,100,10)),  labels = c(1, seq(10,100,10)))
# dev.off()

#lsa
for (i in 1:length(lsaraw_BN_p)) {
  print(paste0(i, ": ", which(lsaraw_dot_BN[i, ] == max(lsaraw_dot_BN[i, ]))))
}

#multiple for 44, 45, 46, 47
for (i in 1:length(lsaraw_BN_p)) {
  print(paste0(i, ": ", which(lsanrowl1_dot_BN[i, ] == max(lsanrowl1_dot_BN[i, ]))))
}

#multiple for 47

for (i in 1:length(lsaraw_BN_p)) {
  print(paste0(i, ": ", which(lsanrowl2_dot_BN[i, ] == max(lsanrowl2_dot_BN[i, ]))))
}

#multiple for 43


for (i in 1:length(lsaraw_BN_p)) {
  print(paste0(i, ": ", which(lsatfidf_dot_BN[i, ] == max(lsatfidf_dot_BN[i, ]))))
}

#ca
for (i in 1:length(caraw_BN_p)) {
  print(paste0(i, ": ", which(caraw_dot_BN[i, ] == max(caraw_dot_BN[i, ]))))
}

for (i in 1:length(caraw_BN_p)) {
  print(paste0(i, ": ", which(canrowl1_dot_BN[i, ] == max(canrowl1_dot_BN[i, ]))))
}

for (i in 1:length(caraw_BN_p)) {
  print(paste0(i, ": ", which(canrowl2_dot_BN[i, ] == max(canrowl2_dot_BN[i, ]))))
}

for (i in 1:length(caraw_BN_p)) {
  print(paste0(i, ": ", which(catfidf_dot_BN[i, ] == max(catfidf_dot_BN[i, ]))))
}



lsaraw_BN_p[which.max(max_p_dot_lsaraw_BN[1,])]
lsaraw_BN_p[which.max(max_p_dot_lsanrowl1_BN[1,])]
lsaraw_BN_p[which.max(max_p_dot_lsanrowl2_BN[1,])]
lsaraw_BN_p[which.max(max_p_dot_lsatfidf_BN[1,])]

which(max_p_dot_lsaraw_BN[1,] == max(max_p_dot_lsaraw_BN[1,]))
which(max_p_dot_lsanrowl1_BN[1,] == max(max_p_dot_lsanrowl1_BN[1,]))
which(max_p_dot_lsanrowl2_BN[1,] == max(max_p_dot_lsanrowl2_BN[1,]))
which(max_p_dot_lsatfidf_BN[1,] == max(max_p_dot_lsatfidf_BN[1,]))

sum(lsaraw_dot_BN == max(lsaraw_dot_BN))
sum(lsanrowl1_dot_BN == max(lsanrowl1_dot_BN))
sum(lsanrowl2_dot_BN == max(lsanrowl2_dot_BN))
sum(lsatfidf_dot_BN == max(lsatfidf_dot_BN))

lsaraw_BN_p[which(lsaraw_dot_BN == max(lsaraw_dot_BN), arr.ind = TRUE)[1]]
lsaraw_BN_p[which(lsanrowl1_dot_BN == max(lsanrowl1_dot_BN), arr.ind = TRUE)[1]]
lsaraw_BN_p[which(lsanrowl2_dot_BN == max(lsanrowl2_dot_BN), arr.ind = TRUE)[1]]
lsaraw_BN_p[which(lsatfidf_dot_BN == max(lsatfidf_dot_BN), arr.ind = TRUE)[1]]

lsaraw_BN_dim[which(lsaraw_dot_BN == max(lsaraw_dot_BN), arr.ind = TRUE)[2]]
lsaraw_BN_dim[which(lsanrowl1_dot_BN == max(lsanrowl1_dot_BN), arr.ind = TRUE)[2]]
lsaraw_BN_dim[which(lsanrowl2_dot_BN == max(lsanrowl2_dot_BN), arr.ind = TRUE)[2]]
lsaraw_BN_dim[which(lsatfidf_dot_BN == max(lsatfidf_dot_BN), arr.ind = TRUE)[2]]

caraw_BN_p[which.max(max_p_dot_caraw_BN[1,])]
caraw_BN_p[which.max(max_p_dot_canrowl1_BN[1,])]
caraw_BN_p[which.max(max_p_dot_canrowl2_BN[1,])]
caraw_BN_p[which.max(max_p_dot_catfidf_BN[1,])]

which(max_p_dot_caraw_BN[1,] == max(max_p_dot_caraw_BN[1,]))
which(max_p_dot_canrowl1_BN[1,] == max(max_p_dot_canrowl1_BN[1,]))
which(max_p_dot_canrowl2_BN[1,] == max(max_p_dot_canrowl2_BN[1,]))
which(max_p_dot_catfidf_BN[1,] == max(max_p_dot_catfidf_BN[1,]))

sum(caraw_dot_BN == max(caraw_dot_BN))
sum(canrowl1_dot_BN == max(canrowl1_dot_BN))
sum(canrowl2_dot_BN == max(canrowl2_dot_BN))
sum(catfidf_dot_BN == max(catfidf_dot_BN))

lsaraw_BN_p[which(caraw_dot_BN == max(caraw_dot_BN), arr.ind = TRUE)[1]]
lsaraw_BN_p[which(canrowl1_dot_BN == max(canrowl1_dot_BN), arr.ind = TRUE)[1]]
lsaraw_BN_p[which(canrowl2_dot_BN == max(canrowl2_dot_BN), arr.ind = TRUE)[1]]
lsaraw_BN_p[which(catfidf_dot_BN == max(catfidf_dot_BN), arr.ind = TRUE)[1]]

lsaraw_BN_dim[which(caraw_dot_BN == max(caraw_dot_BN), arr.ind = TRUE)[2]]
lsaraw_BN_dim[which(canrowl1_dot_BN == max(canrowl1_dot_BN), arr.ind = TRUE)[2]]
lsaraw_BN_dim[which(canrowl2_dot_BN == max(canrowl2_dot_BN), arr.ind = TRUE)[2]]
lsaraw_BN_dim[which(catfidf_dot_BN == max(catfidf_dot_BN), arr.ind = TRUE)[2]]

plotrange <- 1:length(caraw_BN_p)

setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\figure\\dotchacon\\Fdotoptimalmapbbcnews.eps")
par(mar=c(5,6,4,1)+.1)

plot(caraw_BN_p[plotrange],raw_dot_BN_p, ylim = c(0.2, 0.9),type="b", lty=1, pch = 15, xlab = expression(paste(alpha), " value"), ylab = "MAP (dot)", cex.lab=1.25, xaxt = "n")
points(caraw_BN_p[plotrange],nrowl1_dot_BN_p,col=2,lty=1, pch = 15, cex=1.25)
points(caraw_BN_p[plotrange],nrowl2_dot_BN_p,col=3,lty=1, pch = 15, cex=1.25)
points(caraw_BN_p[plotrange],tfidf_dot_BN_p,col=4,lty=1, pch = 15, cex=1.25)

lines(caraw_BN_p[plotrange],raw_dot_BN_p,col=1,lty=1, pch = 15, cex=1.25)
lines(caraw_BN_p[plotrange],nrowl1_dot_BN_p,col=2,lty=1, pch = 15, cex=1.25)
lines(caraw_BN_p[plotrange],nrowl2_dot_BN_p,col=3,lty=1, pch = 15, cex=1.25)
lines(caraw_BN_p[plotrange],tfidf_dot_BN_p,col=4,lty=1, pch = 15, cex=1.25)


points(caraw_BN_p[plotrange],max_p_dot_lsaraw_BN[1,plotrange],col=1, cex=1.25,lty=2, pch = 16)
points(caraw_BN_p[plotrange],max_p_dot_lsanrowl1_BN[1,plotrange],col=2, cex=1.25,lty=2, pch = 16)
points(caraw_BN_p[plotrange],max_p_dot_lsanrowl2_BN[1,plotrange],col=3, cex=1.25,lty=2, pch = 16)
points(caraw_BN_p[plotrange],max_p_dot_lsatfidf_BN[1,plotrange],col=4, cex=1.25,lty=2, pch = 16)

lines(caraw_BN_p[plotrange],max_p_dot_lsaraw_BN[1,plotrange],col=1, cex=1.25,lty=2, pch = 16)
lines(caraw_BN_p[plotrange],max_p_dot_lsanrowl1_BN[1,plotrange],col=2, cex=1.25,lty=2, pch = 16)
lines(caraw_BN_p[plotrange],max_p_dot_lsanrowl2_BN[1,plotrange],col=3, cex=1.25,lty=2, pch = 16)
lines(caraw_BN_p[plotrange],max_p_dot_lsatfidf_BN[1,plotrange],col=4, cex=1.25,lty=2, pch = 16)



points(caraw_BN_p[plotrange],max_p_dot_caraw_BN[1,plotrange],col=1, cex=1.25,lty=3, pch = 17)
points(caraw_BN_p[plotrange],max_p_dot_canrowl1_BN[1,plotrange],col=2, cex=1.25,lty=3, pch = 17)
points(caraw_BN_p[plotrange],max_p_dot_canrowl2_BN[1,plotrange],col=3, cex=1.25,lty=3, pch = 17)
points(caraw_BN_p[plotrange],max_p_dot_catfidf_BN[1,plotrange],col=4, cex=1.25,lty=3, pch = 17)

lines(caraw_BN_p[plotrange],max_p_dot_caraw_BN[1,plotrange],col=1, cex=1.25,lty=3, pch = 17)
lines(caraw_BN_p[plotrange],max_p_dot_canrowl1_BN[1,plotrange],col=2, cex=1.25,lty=3, pch = 17)
lines(caraw_BN_p[plotrange],max_p_dot_canrowl2_BN[1,plotrange],col=3, cex=1.25,lty=3, pch = 17)
lines(caraw_BN_p[plotrange],max_p_dot_catfidf_BN[1,plotrange],col=4, cex=1.25,lty=3, pch = 17)

# points(caraw_BN_p[which.max(max_lsaraw_dot_BN)], max(max_lsaraw_dot_BN), col = 1, type = "p", pch = 20)
# points(caraw_BN_p[which.max(max_lsanrowl1_dot_BN)], max(max_lsanrowl1_dot_BN), col = 2, type = "p", pch = 20)
# points(caraw_BN_p[which.max(max_lsanrowl2_dot_BN)], max(max_lsanrowl2_dot_BN), col = 3, type = "p", pch = 20)
# points(caraw_BN_p[which.max(max_lsatfidf_dot_BN)], max(max_lsatfidf_dot_BN), col = 4, type = "p", pch = 20)
# points(caraw_BN_p[which.max(max_p_caraw_dot_BN)], max(max_p_caraw_dot_BN), col = 5, type = "p", pch = 20)
# 
#title(expression(paste("Optimal k (BBCNews)")),lwd=3)
legend("bottomleft", ncol=3,cex=1,c("RAW", "NROWL1", "NROWL2", "TFIDF","LSA-RAW", "LSA-NROWL1", "LSA-NROWL2", "LSA-TFIDF", "CA-RAW", "CA-NROWL1", "CA-NROWL2", "CA-TFIDF"),col=c(1,2,3,4,1,2,3,4,1,2,3,4), lty = c(1,1,1,1,2,2,2,2,3,3,3,3), pch = c(15,15,15,15,16,16,16,16,17,17,17,17))
#,inset=c(0,0.4)
axis(side = 1, at = lsaraw_BN_p,  labels = FALSE)
axis(side = 1, at = seq(-6, 8, 1),  labels = seq(-6, 8, 1))
dev.off()




