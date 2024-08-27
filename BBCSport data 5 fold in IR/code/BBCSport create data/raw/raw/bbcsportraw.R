# The matrix which is loaded is Copied from the calsa-tc/BBCSport data 5 fold in/created data/created matrix/. In order to run the code in the same folder, we modify the path of data


rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
#load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data\\rawdtmBBCsport.Rdata")
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\created matrix\\rawdtmBBCsport.Rdata")

source("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\meanprerawnrowtfidfpvalue.R")

loo.len  = 1:length(testing_dtm)
p <- c(seq(-6, -2, by=0.5), seq(-1.8, 3.8, by=0.2), seq(4, 8, by=0.5))

euc_sum_temp <- cos_sum_temp <- dot_sum_temp <- matrix(0, nrow = 1, ncol = 1)

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
  
  
  euc_raw_BS <- cos_raw_BS <- dot_raw_BS <- matrix(NA, nrow = length(p), ncol = 1)

  dis_metric <- "all"
  

  row.names(training_dtm[[n]])  <- training_index[[n]]
  raw_BS <- meanprerawnrowtfidfpvalue(training_dtm[[n]], testing_dtm[[n]], training_index[[n]], testing_index[[n]], metric = dis_metric, retrieved_document_number = nrow_dtm_training, recall_value)
  euc_raw_BS <- raw_BS[[1]]
  cos_raw_BS <- raw_BS[[2]]
  dot_raw_BS <- raw_BS[[3]]
  # print(euc_raw_BS[i,])
  # print(cos_raw_BS[i,])
  # print(dot_raw_BS[i,])
  
  euc_sum_temp <- euc_sum_temp + euc_raw_BS
  cos_sum_temp <- cos_sum_temp + cos_raw_BS
  dot_sum_temp <- dot_sum_temp + dot_raw_BS
}
raw_euc_BS <- euc_sum_temp/length(testing_dtm)
raw_cos_BS <- cos_sum_temp/length(testing_dtm)
raw_dot_BS <- dot_sum_temp/length(testing_dtm)



raw_BS_p <- p
save(raw_BS_p, raw_euc_BS,raw_cos_BS, raw_dot_BS, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\raw\\raw\\meanprerawBS.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    
