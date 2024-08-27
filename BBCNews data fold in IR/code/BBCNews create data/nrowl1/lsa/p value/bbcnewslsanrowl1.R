rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)

load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\nrowL1dtmBBCnews.Rdata")

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


#lsanrowl1
dtm.training.lsanrowl1.svd <- svd(training_dtm)
# round(dtm.training.lsanrowl1.svd$d)
lsanrowl1_BN <- list()
lsanrowl1_euc_BN <- lsanrowl1_cos_BN <- lsanrowl1_dot_BN <- matrix(NA, nrow = length(p), ncol = length(dim))

dis_metric <- "all"

for (i in 1:length(p)) {
  print(p[i])
  dtm.training.lsanrowl1.svd.ud <- as.matrix(dtm.training.lsanrowl1.svd$u)%*%as.matrix(diag((dtm.training.lsanrowl1.svd$d)^(p[i])))
  dtm.testing.lsanrowl1.svd.ud <- as.matrix(testing_dtm) %*% as.matrix(dtm.training.lsanrowl1.svd$v)%*%as.matrix(diag((dtm.training.lsanrowl1.svd$d)^(p[i]-1)))
  row.names(dtm.training.lsanrowl1.svd.ud)  <- training_index
  lsanrowl1_BN[[i]] <- meanprelsacapvalue(dtm.training.lsanrowl1.svd.ud, dtm.testing.lsanrowl1.svd.ud, training_index, testing_index, metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
  lsanrowl1_euc_BN[i,] <- lsanrowl1_BN[[i]][[1]]
  lsanrowl1_cos_BN[i,] <- lsanrowl1_BN[[i]][[2]]
  lsanrowl1_dot_BN[i,] <- lsanrowl1_BN[[i]][[3]]
  }

lsanrowl1_BN_p <- p
lsanrowl1_BN_dim <- dim

save(lsanrowl1_BN_dim, lsanrowl1_BN_p, lsanrowl1_euc_BN,lsanrowl1_cos_BN, lsanrowl1_dot_BN, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl1\\lsa\\p value\\meanprelsanrowl1BN.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

