rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\nrowL1dtmBBCnews.Rdata")

source("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\meanprerawnrowtfidfpvalue.R")

p <- c(seq(-6, -2, by=0.5), seq(-1.8, 3.8, by=0.2), seq(4, 8, by=0.5))


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



dis_metric <- "all"


row.names(training_dtm)  <- training_index
nrowl1_BN <- meanprerawnrowtfidfpvalue(training_dtm, testing_dtm, training_index, testing_index, metric = dis_metric, retrieved_document_number = nrow_dtm_training, recall_value)
nrowl1_euc_BN <- nrowl1_BN[[1]]
nrowl1_cos_BN <- nrowl1_BN[[2]]
nrowl1_dot_BN <- nrowl1_BN[[3]]


nrowl1_BN_p <- p
save(nrowl1_BN_p, nrowl1_euc_BN,nrowl1_cos_BN, nrowl1_dot_BN, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\nrowl1\\nrowl1\\meanprenrowl1BN.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    
