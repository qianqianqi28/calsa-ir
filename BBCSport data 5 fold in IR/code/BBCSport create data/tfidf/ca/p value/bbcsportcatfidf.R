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
  
  #catfidf
  ###
  training_dtm.P    <- training_dtm[[n]]/sum(training_dtm[[n]])
  ### Row and column masses
  training_dtm.r    <- apply(training_dtm.P, 1, sum)
  training_dtm.c    <- apply(training_dtm.P, 2, sum)
  ### CA Step 1: the matrix S
  training_dtm.Dr   <- diag(training_dtm.r)
  training_dtm.Dc   <- diag(training_dtm.c)
  training_dtm.Drmh <- diag(1/sqrt(training_dtm.r))
  training_dtm.Dcmh <- diag(1/sqrt(training_dtm.c))
  
  training_dtm.SR   <- training_dtm.Drmh%*%(training_dtm.P-training_dtm.r%o%training_dtm.c)%*%training_dtm.Dcmh
  
  dtm.training.catfidf.svd <- svd(training_dtm.SR)
  
  # round(dtm.training.catfidf.svd$d)
  catfidf_BS <- list()
  euc_catfidf_BS <- cos_catfidf_BS <- dot_catfidf_BS <- matrix(NA, nrow = length(p), ncol = length(dim))
  # max_euc_catfidf_BS <- max_cos_catfidf_BS <- max_dot_catfidf_BS <- matrix(NA, nrow = 1, ncol = length(p))
  
  dis_metric <- "all"
  temp <- testing_dtm[[n]]
  for (q in 1:nrow(temp)){
    temp[q,] <- t(as.matrix(temp[q,])/sum(as.matrix(temp[q,])))
  }
  
  for (i in 1:length(p)) {
    print(p[i])
  
    dtm.training.catfidf.svd.ud <- as.matrix(training_dtm.Drmh%*%as.matrix(dtm.training.catfidf.svd$u)%*%as.matrix(diag((dtm.training.catfidf.svd$d)^(p[i]))))
    
    training_dtm.csc <- training_dtm.Dcmh%*%dtm.training.catfidf.svd$v%*%as.matrix(diag((dtm.training.catfidf.svd$d)^(p[i]-1)))
    
    dtm.testing.catfidf.svd.ud <- as.matrix(temp) %*%as.matrix(training_dtm.csc)
    
    row.names(dtm.training.catfidf.svd.ud)  <- training_index[[n]]
    
    
    catfidf_BS[[i]] <- meanprelsacapvalue(dtm.training.catfidf.svd.ud, dtm.testing.catfidf.svd.ud, training_index[[n]], testing_index[[n]], metric = dis_metric, dim, retrieved_document_number = nrow_dtm_training, recall_value)
    euc_catfidf_BS[i,] <- catfidf_BS[[i]][[1]]
    cos_catfidf_BS[i,] <- catfidf_BS[[i]][[2]]
    dot_catfidf_BS[i,] <- catfidf_BS[[i]][[3]]
    # print(euc_catfidf_BS[i,])
    # print(cos_catfidf_BS[i,])
    # print(dot_catfidf_BS[i,])
    }
  euc_sum_temp <- euc_sum_temp + euc_catfidf_BS
  cos_sum_temp <- cos_sum_temp + cos_catfidf_BS
  dot_sum_temp <- dot_sum_temp + dot_catfidf_BS
}
catfidf_euc_BS <- euc_sum_temp/length(testing_dtm)
catfidf_cos_BS <- cos_sum_temp/length(testing_dtm)
catfidf_dot_BS <- dot_sum_temp/length(testing_dtm)



catfidf_BS_p <- p
catfidf_BS_dim <- dim
save(catfidf_BS_dim, catfidf_BS_p, catfidf_euc_BS,catfidf_cos_BS, catfidf_dot_BS, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\tfidf\\ca\\p value\\meanprecatfidfBS.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

