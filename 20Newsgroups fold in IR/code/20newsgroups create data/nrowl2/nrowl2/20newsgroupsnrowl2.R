rm(list=ls())
timestart<-Sys.time()
library(Matrix)
library(lsa)
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\20Newsgroups fold in IR\\created data\\created matrix\\nrowL2dtm20newsgroups.Rdata")

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
nrowl2_20NG <- meanprerawnrowtfidfpvalue(training_dtm, testing_dtm, training_index, testing_index, metric = dis_metric, retrieved_document_number = nrow_dtm_training, recall_value)
nrowl2_euc_20NG <- nrowl2_20NG[[1]]
nrowl2_cos_20NG <- nrowl2_20NG[[2]]
nrowl2_dot_20NG <- nrowl2_20NG[[3]]


nrowl2_20NG_p <- p
save(nrowl2_20NG_p, nrowl2_euc_20NG,nrowl2_cos_20NG, nrowl2_dot_20NG, file='C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\20Newsgroups fold in IR\\created data\\nrowl2\\nrowl2\\meanprenrowl220NG.Rdata')

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)    
