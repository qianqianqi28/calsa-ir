#The dataset is the same as the dataset used in the first paper. In the first paper, we used five-fold cross validation; here we do not use five-fold cross validation

rm(list=ls())

timestart<-Sys.time()


load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\rawdtmBBCnews.Rdata")

#TF matrix
TF <- as.matrix(training_dtm) 
dim(TF)

#IDF matrix
pre.IDF <- training_dtm
pre.IDF[pre.IDF > 0.5] <- 1
IDF <- log(nrow(pre.IDF)/colSums(pre.IDF),base = 2)

#plus 1
IDF <- IDF +1
IDF <- diag(IDF)

#TF-IDF matrix
training_dtm <- TF %*% IDF
dim(training_dtm)
testing_dtm <- testing_dtm%*% IDF

dim(training_dtm)
dim(testing_dtm)
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\tfidfdtmBBCnews.Rdata")
timeend<-Sys.time()
timeend- timestart
