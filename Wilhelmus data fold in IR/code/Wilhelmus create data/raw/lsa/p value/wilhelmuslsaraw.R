# The matrix wihch is loaded is Copied from the calsa-tc/Wilhelmus data fold in/created data/created matrix/. In order to run the code in the same folder, we modify the path of data


rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
#load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data\\rawdtmBBCsport.Rdata")
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\rawdtmWilhelmusdataset.Rdata")

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
  
  
  #lsaraw
  dtm.training.lsaraw.svd <- svd(training_dtm[[n]])
  # round(dtm.training.lsaraw.svd$d)
  lsaraw_WH <- list()
  euc_lsaraw_WH <- cos_lsaraw_WH <- dot_lsaraw_WH <- matrix(NA, nrow = length(p), ncol = length(dim))
  # max_euc_lsaraw_WH <- max_cos_lsaraw_WH <- max_dot_lsaraw_WH <- matrix(NA, nrow = 1, ncol = length(p))
  
  dis_metric <- "all"
  
  for (i in 1:length(p)) {
    #print(p[i])
    dtm.training.lsaraw.svd.ud <- as.matrix(dtm.training.lsaraw.svd$u)%*%as.matrix(diag((dtm.training.lsaraw.svd$d)^(p[i])))
    dtm.testing.lsaraw.svd.ud <- as.matrix(testing_dtm[[n]]) %*% as.matrix(dtm.training.lsaraw.svd$v)%*%as.matrix(diag((dtm.training.lsaraw.svd$d)^(p[i]-1)))
    row.names(dtm.training.lsaraw.svd.ud)  <- training_index[[n]]
    lsaraw_WH[[i]] <- whmeanprelsacapvalue(dtm.training.lsaraw.svd.ud, dtm.testing.lsaraw.svd.ud, training_index[[n]], testing_index[[n]], metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
    euc_lsaraw_WH[i,] <- lsaraw_WH[[i]][[1]]
    cos_lsaraw_WH[i,] <- lsaraw_WH[[i]][[2]]
    dot_lsaraw_WH[i,] <- lsaraw_WH[[i]][[3]]
    # print(euc_lsaraw_WH[i,])
    # print(cos_lsaraw_WH[i,])
    # print(dot_lsaraw_WH[i,])
    }
  euc_sum_temp <- euc_sum_temp + euc_lsaraw_WH
  cos_sum_temp <- cos_sum_temp + cos_lsaraw_WH
  dot_sum_temp <- dot_sum_temp + dot_lsaraw_WH
}
lsaraw_euc_WH <- euc_sum_temp/length(testing_dtm)
lsaraw_cos_WH <- cos_sum_temp/length(testing_dtm)
lsaraw_dot_WH <- dot_sum_temp/length(testing_dtm)



lsaraw_WH_p <- p
lsaraw_WH_dim <- dim
save(lsaraw_WH_dim, lsaraw_WH_p, lsaraw_euc_WH,lsaraw_cos_WH, lsaraw_dot_WH, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\raw\\lsa\\p value\\meanprelsarawWH.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

