# The matrix which is loaded is Copied from the calsa-tc/BBCSport data 5 fold in/created data/created matrix/. In order to run the code in the same folder, we modify the path of data


rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
#load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data\\nrowL1dtmBBCsport.Rdata")
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\created matrix\\nrowL1dtmBBCsport.Rdata")

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
  
  
  #lsanrowl1
  dtm.training.lsanrowl1.svd <- svd(training_dtm[[n]])
  # round(dtm.training.lsanrowl1.svd$d)
  lsanrowl1_BS <- list()
  euc_lsanrowl1_BS <- cos_lsanrowl1_BS <- dot_lsanrowl1_BS <- matrix(NA, nrow = length(p), ncol = length(dim))
  # max_euc_lsanrowl1_BS <- max_cos_lsanrowl1_BS <- max_dot_lsanrowl1_BS <- matrix(NA, nrow = 1, ncol = length(p))
  
  dis_metric <- "all"
  
  for (i in 1:length(p)) {
    print(p[i])
    dtm.training.lsanrowl1.svd.ud <- as.matrix(dtm.training.lsanrowl1.svd$u)%*%as.matrix(diag((dtm.training.lsanrowl1.svd$d)^(p[i])))
    dtm.testing.lsanrowl1.svd.ud <- as.matrix(testing_dtm[[n]]) %*% as.matrix(dtm.training.lsanrowl1.svd$v)%*%as.matrix(diag((dtm.training.lsanrowl1.svd$d)^(p[i]-1)))
    row.names(dtm.training.lsanrowl1.svd.ud)  <- training_index[[n]]
    lsanrowl1_BS[[i]] <- meanprelsacapvalue(dtm.training.lsanrowl1.svd.ud, dtm.testing.lsanrowl1.svd.ud, training_index[[n]], testing_index[[n]], metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
    euc_lsanrowl1_BS[i,] <- lsanrowl1_BS[[i]][[1]]
    cos_lsanrowl1_BS[i,] <- lsanrowl1_BS[[i]][[2]]
    dot_lsanrowl1_BS[i,] <- lsanrowl1_BS[[i]][[3]]
    # print(euc_lsanrowl1_BS[i,])
    # print(cos_lsanrowl1_BS[i,])
    # print(dot_lsanrowl1_BS[i,])
    }
  euc_sum_temp <- euc_sum_temp + euc_lsanrowl1_BS
  cos_sum_temp <- cos_sum_temp + cos_lsanrowl1_BS
  dot_sum_temp <- dot_sum_temp + dot_lsanrowl1_BS
}
lsanrowl1_euc_BS <- euc_sum_temp/length(testing_dtm)
lsanrowl1_cos_BS <- cos_sum_temp/length(testing_dtm)
lsanrowl1_dot_BS <- dot_sum_temp/length(testing_dtm)



lsanrowl1_BS_p <- p
lsanrowl1_BS_dim <- dim
save(lsanrowl1_BS_dim, lsanrowl1_BS_p, lsanrowl1_euc_BS,lsanrowl1_cos_BS, lsanrowl1_dot_BS, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\nrowl1\\lsa\\p value\\meanprelsanrowl1BS.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

