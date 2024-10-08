#Same as the calsa-tc/Wilhelmus data fold in/code/Wilhelmus create matrix/nrowL1dtmWilhelmusdataset.R


#Copy from the calsa-tc/Wilhelmus data fold in/code/Wilhelmus create matrix/nrowL1dtmWilhelmusdataset.R. In order to run the code in the same folder, we modify the path of data


#This code is for songs from six authors using LOOCV. Note each time 185 documents from training set form a nrowl1 document-term matrix of size 185*300 and the single document of validation set is not included in the matrix.



rm(list=ls())
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\rawdtmWilhelmusdataset.Rdata")

loo.len  = 1:186

for(i in loo.len){
  training_dtm[[i]] <- training_dtm[[i]]/matrix(rep(apply(training_dtm[[i]],1,sum),each = ncol(training_dtm[[i]])), ncol = ncol(training_dtm[[i]]), by = TRUE)
  testing_dtm[[i]] <- testing_dtm[[i]]/matrix(rep(apply(testing_dtm[[i]],1,sum),each = ncol(testing_dtm[[i]])), ncol = ncol(testing_dtm[[i]]), by = TRUE)
  apply(training_dtm[[i]], 1, sum)
  apply(testing_dtm[[i]], 1, sum)
}

save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\nrowL1dtmWilhelmusdataset.Rdata")
