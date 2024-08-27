# The matrix wihch is loaded is Copied from the calsa-tc/Wilhelmus data fold in/created data/created matrix/. In order to run the code in the same folder, we modify the path of data


rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
#load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data\\nrowl2dtmBBCsport.Rdata")
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\nrowL2dtmWilhelmusdataset.Rdata")

source("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\whmeanprelsacapvalue.R")

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
  
  
  #lsanrowl2
  dtm.training.lsanrowl2.svd <- svd(training_dtm[[n]])
  # round(dtm.training.lsanrowl2.svd$d)
  lsanrowl2_WH <- list()
  euc_lsanrowl2_WH <- cos_lsanrowl2_WH <- dot_lsanrowl2_WH <- matrix(NA, nrow = length(p), ncol = length(dim))
  # max_euc_lsanrowl2_WH <- max_cos_lsanrowl2_WH <- max_dot_lsanrowl2_WH <- matrix(NA, nrow = 1, ncol = length(p))
  
  dis_metric <- "all"
  
  for (i in 1:length(p)) {
    #print(p[i])
    dtm.training.lsanrowl2.svd.ud <- as.matrix(dtm.training.lsanrowl2.svd$u)%*%as.matrix(diag((dtm.training.lsanrowl2.svd$d)^(p[i])))
    dtm.testing.lsanrowl2.svd.ud <- as.matrix(testing_dtm[[n]]) %*% as.matrix(dtm.training.lsanrowl2.svd$v)%*%as.matrix(diag((dtm.training.lsanrowl2.svd$d)^(p[i]-1)))
    row.names(dtm.training.lsanrowl2.svd.ud)  <- training_index[[n]]
    lsanrowl2_WH[[i]] <- whmeanprelsacapvalue(dtm.training.lsanrowl2.svd.ud, dtm.testing.lsanrowl2.svd.ud, training_index[[n]], testing_index[[n]], metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
    euc_lsanrowl2_WH[i,] <- lsanrowl2_WH[[i]][[1]]
    cos_lsanrowl2_WH[i,] <- lsanrowl2_WH[[i]][[2]]
    dot_lsanrowl2_WH[i,] <- lsanrowl2_WH[[i]][[3]]
    # print(euc_lsanrowl2_WH[i,])
    # print(cos_lsanrowl2_WH[i,])
    # print(dot_lsanrowl2_WH[i,])
  }
  euc_sum_temp <- euc_sum_temp + euc_lsanrowl2_WH
  cos_sum_temp <- cos_sum_temp + cos_lsanrowl2_WH
  dot_sum_temp <- dot_sum_temp + dot_lsanrowl2_WH
}
lsanrowl2_euc_WH <- euc_sum_temp/length(testing_dtm)
lsanrowl2_cos_WH <- cos_sum_temp/length(testing_dtm)
lsanrowl2_dot_WH <- dot_sum_temp/length(testing_dtm)



lsanrowl2_WH_p <- p
lsanrowl2_WH_dim <- dim
save(lsanrowl2_WH_dim, lsanrowl2_WH_p, lsanrowl2_euc_WH,lsanrowl2_cos_WH, lsanrowl2_dot_WH, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\nrowl2\\lsa\\p value\\meanprelsanrowl2WH.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

