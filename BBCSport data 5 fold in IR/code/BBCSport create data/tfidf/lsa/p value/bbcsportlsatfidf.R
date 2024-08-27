# The matrix which is loaded is Copied from the calsa-tc/BBCSport data 5 fold in/created data/created matrix/. In order to run the code in the same folder, we modify the path of data


rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
#load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data\\tfidfdtmBBCsport.Rdata")
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\created matrix\\tfidfdtmBBCsport.Rdata")

source("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\meanprelsacapvalue.R")

loo.len  = 1:length(testing_dtm)
p <- c(seq(-6, -2, by=0.5), seq(-1.8, 3.8, by=0.2), seq(4, 8, by=0.5))
dim <- c(seq(1, 19, by=1),seq(20, 48, by=2), seq(50, 100, by=10))
# p <- c(seq(-6, -1.2, by=0.2), seq(-1, 2.9, by=0.1), seq(3, 8, by=0.2))
# dim <- c(seq(1, 49, by=1), seq(50, 98, by=2), seq(100, 200, by=5))
#p <- seq(-4, 6, by=0.1)
#dim <- c(1:rankMatrix(training_dtm))
# p <- c(-4,-3.9)
# dim <- c(1,2,3)
euc_sum_temp <- cos_sum_temp <- dot_sum_temp <- matrix(0, nrow = length(p), ncol = length(dim))

for (n in loo.len){
  print(n)
  nrow_dtm_training <- nrow(training_dtm[[n]])
  
  recall_value <- matrix(NA, nrow = 1, ncol = length(testing_index[[n]]))
  
  for (i in 1:length(testing_index[[n]])){
    if (testing_index[[n]][i] == 1) {
      recall_value[1,i] = sum(training_index[[n]] == 1)
    } else if (testing_index[[n]][i] == 2) {
      recall_value[1,i] = sum(training_index[[n]] == 2)
    } else if (testing_index[[n]][i] == 3) {
      recall_value[1,i] = sum(training_index[[n]] == 3)
    } else if (testing_index[[n]][i] == 4) {
      recall_value[1,i] = sum(training_index[[n]] == 4)
    } else if (testing_index[[n]][i] == 5) {
      recall_value[1,i]= sum(training_index[[n]] == 5)
    } else if (testing_index[[n]][i] == 6) {
      recall_value[1,i] = sum(training_index[[n]] == 6)
    }
  }
  
  
  #lsatfidf
  dtm.training.lsatfidf.svd <- svd(training_dtm[[n]])
  # round(dtm.training.lsatfidf.svd$d)
  lsatfidf_BS <- list()
  euc_lsatfidf_BS <- cos_lsatfidf_BS <- dot_lsatfidf_BS <- matrix(NA, nrow = length(p), ncol = length(dim))
  # max_euc_lsatfidf_BS <- max_cos_lsatfidf_BS <- max_dot_lsatfidf_BS <- matrix(NA, nrow = 1, ncol = length(p))
  
  dis_metric <- "all"
  
  for (i in 1:length(p)) {
    print(p[i])
    dtm.training.lsatfidf.svd.ud <- as.matrix(dtm.training.lsatfidf.svd$u)%*%as.matrix(diag((dtm.training.lsatfidf.svd$d)^(p[i])))
    dtm.testing.lsatfidf.svd.ud <- as.matrix(testing_dtm[[n]]) %*% as.matrix(dtm.training.lsatfidf.svd$v)%*%as.matrix(diag((dtm.training.lsatfidf.svd$d)^(p[i]-1)))
    row.names(dtm.training.lsatfidf.svd.ud)  <- training_index[[n]]
    lsatfidf_BS[[i]] <- meanprelsacapvalue(dtm.training.lsatfidf.svd.ud, dtm.testing.lsatfidf.svd.ud, training_index[[n]], testing_index[[n]], metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
    euc_lsatfidf_BS[i,] <- lsatfidf_BS[[i]][[1]]
    cos_lsatfidf_BS[i,] <- lsatfidf_BS[[i]][[2]]
    dot_lsatfidf_BS[i,] <- lsatfidf_BS[[i]][[3]]
    # print(euc_lsatfidf_BS[i,])
    # print(cos_lsatfidf_BS[i,])
    # print(dot_lsatfidf_BS[i,])
    }
  euc_sum_temp <- euc_sum_temp + euc_lsatfidf_BS
  cos_sum_temp <- cos_sum_temp + cos_lsatfidf_BS
  dot_sum_temp <- dot_sum_temp + dot_lsatfidf_BS
}
lsatfidf_euc_BS <- euc_sum_temp/length(testing_dtm)
lsatfidf_cos_BS <- cos_sum_temp/length(testing_dtm)
lsatfidf_dot_BS <- dot_sum_temp/length(testing_dtm)



lsatfidf_BS_p <- p
lsatfidf_BS_dim <- dim
save(lsatfidf_BS_dim, lsatfidf_BS_p, lsatfidf_euc_BS,lsatfidf_cos_BS, lsatfidf_dot_BS, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\tfidf\\lsa\\p value\\meanprelsatfidfBS.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

