#For the second paper: We randomly choose 600 documents from the training set of four categories (comp.graphics, rec.sport.hockey, sci.crypt, and talk.politics.guns) and 400 documents from the test set of these four categories.


rm(list=ls())
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\20Newsgroups fold in IR\\created data\\created matrix\\rawdtm20newsgroups.Rdata")

dim(training_dtm)
dim(testing_dtm)


sum(training_index==1)
sum(training_index==2)
sum(training_index==3)
sum(training_index==4)
sum(training_index==5)

sum(testing_index==1)
sum(testing_index==2)
sum(testing_index==3)
sum(testing_index==4)
sum(testing_index==5)

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
dim(testing_dtm)

save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\20Newsgroups fold in IR\\created data\\created matrix\\tfidfdtm20newsgroups.Rdata")


