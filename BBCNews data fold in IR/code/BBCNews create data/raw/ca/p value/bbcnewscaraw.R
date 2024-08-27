rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)

load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\rawdtmBBCnews.Rdata")

source("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\meanprelsacapvalue.R")

#p <- c(seq(-6, -1.2, by=0.2), seq(-1, 2.9, by=0.1), seq(3, 8, by=0.2))
#dim <- c(seq(1, 49, by=1), seq(50, 98, by=2), seq(100, 200, by=5))
p <- c(seq(-6, -2, by=0.5), seq(-1.8, 3.8, by=0.2), seq(4, 8, by=0.5))
dim <- c(seq(1, 49, by=1), seq(50, 100, by=5))

#p <- seq(-4, 6, by=0.1)
#dim <- c(1:rankMatrix(training_dtm))
# p <- c(-4,-3.9)
# dim <- c(1,2,3)



nrow_dtm_training <- nrow(training_dtm)

recall_value <- matrix(NA, nrow = 1, ncol = length(testing_index))
for (i in 1:length(testing_index)){
  if (testing_index[i] == 1) {
    recall_value[1,i] = sum(training_index == 1)
  } else if (testing_index[i] == 2) {
    recall_value[1,i] = sum(training_index == 2)
  } else if (testing_index[i] == 3) {
    recall_value[1,i] = sum(training_index == 3)
  } else if (testing_index[i] == 4) {
    recall_value[1,i] = sum(training_index == 4)
  } else if (testing_index[i] == 5) {
    recall_value[1,i]= sum(training_index == 5)
  } else if (testing_index[i] == 6) {
    recall_value[1,i] = sum(training_index == 6)
  }
}


#caraw
###
training_dtm.P    <- training_dtm/sum(training_dtm)
### Row and column masses
training_dtm.r    <- apply(training_dtm.P, 1, sum)
training_dtm.c    <- apply(training_dtm.P, 2, sum)
### CA Step 1: the matrix S
training_dtm.Dr   <- diag(training_dtm.r)
training_dtm.Dc   <- diag(training_dtm.c)
training_dtm.Drmh <- diag(1/sqrt(training_dtm.r))
training_dtm.Dcmh <- diag(1/sqrt(training_dtm.c))

training_dtm.SR   <- training_dtm.Drmh%*%(training_dtm.P-training_dtm.r%o%training_dtm.c)%*%training_dtm.Dcmh

dtm.training.caraw.svd <- svd(training_dtm.SR)
# round(dtm.training.caraw.svd$d)
caraw_euc_BN <- caraw_cos_BN <- caraw_dot_BN <- matrix(NA, nrow = length(p), ncol = length(dim))

dis_metric <- "all"
temp <- testing_dtm
for (q in 1:nrow(temp)){
  temp[q,] <- t(as.matrix(temp[q,])/sum(as.matrix(temp[q,])))
}

for (i in 1:length(p)) {
  print(p[i])
  
  dtm.training.caraw.svd.ud <- as.matrix(training_dtm.Drmh%*%as.matrix(dtm.training.caraw.svd$u)%*%as.matrix(diag((dtm.training.caraw.svd$d)^(p[i]))))
  
  training_dtm.csc <- training_dtm.Dcmh%*%dtm.training.caraw.svd$v%*%as.matrix(diag((dtm.training.caraw.svd$d)^(p[i]-1)))
  
  dtm.testing.caraw.svd.ud <- as.matrix(temp) %*%as.matrix(training_dtm.csc)
  
  row.names(dtm.training.caraw.svd.ud)  <- training_index
  caraw_BN <- meanprelsacapvalue(dtm.training.caraw.svd.ud, dtm.testing.caraw.svd.ud, training_index, testing_index, metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
  caraw_euc_BN[i,] <- caraw_BN[[1]]
  caraw_cos_BN[i,] <- caraw_BN[[2]]
  caraw_dot_BN[i,] <- caraw_BN[[3]]
  }

caraw_BN_p <- p
caraw_BN_dim <- dim

save(caraw_BN_dim, caraw_BN_p, caraw_euc_BN,caraw_cos_BN, caraw_dot_BN, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\raw\\ca\\p value\\meanprecarawBN.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

