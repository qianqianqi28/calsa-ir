# The matrix wihch is loaded is Copied from the calsa-tc/Wilhelmus data fold in/created data/created matrix/. In order to run the code in the same folder, we modify the path of data


rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\tfidfdtmWilhelmusdataset.Rdata")

source("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\whmeanprerawnrowtfidfpvalue.R")

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
  
  
  euc_tfidf_WH <- cos_tfidf_WH <- dot_tfidf_WH <- matrix(NA, nrow = length(p), ncol = 1)
  
  dis_metric <- "all"
  
  
  row.names(training_dtm[[n]])  <- training_index[[n]]
  tfidf_WH <- whmeanprerawnrowtfidfpvalue(training_dtm[[n]], testing_dtm[[n]], training_index[[n]], testing_index[[n]], metric = dis_metric, retrieved_document_number = nrow_dtm_training, recall_value)
  euc_tfidf_WH <- tfidf_WH[[1]]
  cos_tfidf_WH <- tfidf_WH[[2]]
  dot_tfidf_WH <- tfidf_WH[[3]]
  # print(euc_tfidf_WH[i,])
  # print(cos_tfidf_WH[i,])
  # print(dot_tfidf_WH[i,])
  
  euc_sum_temp <- euc_sum_temp + euc_tfidf_WH
  cos_sum_temp <- cos_sum_temp + cos_tfidf_WH
  dot_sum_temp <- dot_sum_temp + dot_tfidf_WH
}
tfidf_euc_WH <- euc_sum_temp/length(testing_dtm)
tfidf_cos_WH <- cos_sum_temp/length(testing_dtm)
tfidf_dot_WH <- dot_sum_temp/length(testing_dtm)



tfidf_WH_p <- p
save(tfidf_WH_p, tfidf_euc_WH,tfidf_cos_WH, tfidf_dot_WH, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\tfidf\\tfidf\\meanpretfidfWH.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    

