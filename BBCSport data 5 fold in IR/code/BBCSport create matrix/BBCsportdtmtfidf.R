#Same as the calsa-tc/BBCSport data 5 fold in/code/BBCSport create matrix/BBCsportdtmtfidf.R
#Copy from the calsa-tc/BBCSport data 5 fold in/code/BBCSport create matrix/BBCsportdtmtfidf.R. In order to run the code in the same folder, we modify the path of data
#This code is for 737 documents from five categories using five-fold cross validation. Note each time the documents from the training set form a tfidf document-term matrix and the documents of validation set are not included in the matrix.


rm(list=ls())

timestart<-Sys.time()


load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\created matrix\\rawdtmBBCsport.Rdata")

loo.len  = 1:length(testing_dtm)


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
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCSport data 5 fold in IR\\created data\\created matrix\\tfidfdtmBBCsport.Rdata")
timeend<-Sys.time()
timeend- timestart
