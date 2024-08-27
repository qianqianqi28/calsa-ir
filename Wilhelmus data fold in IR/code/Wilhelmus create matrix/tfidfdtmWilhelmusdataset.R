#Same as the calsa-tc/Wilhelmus data fold in/code/Wilhelmus create matrix/tfidfdtmWilhelmusdataset.R

#Copy from the calsa-tc/Wilhelmus data fold in/code/Wilhelmus create matrix/tfidfdtmWilhelmusdataset.R. In order to run the code in the same folder, we modify the path of data


#This code is for songs from six authors using LOOCV. Note each time 185 documents from training set form a tfidf document-term matrix of size 185*300 and the single document of validation set is not included in the matrix.



rm(list=ls())
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\rawdtmWilhelmusdataset.Rdata")

loo.len  = 1:186

for(i in loo.len){
  #TF matrix
  TF <- as.matrix(training_dtm[[i]]) 
  dim(TF)
  
  #IDF matrix
  pre.IDF <- training_dtm[[i]]
  pre.IDF[pre.IDF > 0.5] <- 1
  IDF <- log(nrow(pre.IDF)/colSums(pre.IDF),base = 2)
  
  #plus 1
  IDF <- IDF +1
  IDF <- diag(IDF)
  
  #TF-IDF matrix
  training_dtm[[i]] <- TF %*% IDF
  dim(training_dtm[[i]])
  testing_dtm[[i]] <- testing_dtm[[i]]%*% IDF

}
dim(training_dtm[[i]])
dim(testing_dtm[[i]])
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\Wilhelmus data fold in IR\\created data\\created matrix\\tfidfdtmWilhelmusdataset.Rdata")

