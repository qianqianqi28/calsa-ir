#The dataset is the same as the dataset used in the first paper. In the first paper, we used five-fold cross validation; here we do not use five-fold cross validation

rm(list=ls())
timestart<-Sys.time()
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\rawdtmBBCnews.Rdata")


training_dtm <- training_dtm/matrix(rep(apply(training_dtm,1,sum),each = ncol(training_dtm)), ncol = ncol(training_dtm), by = TRUE)
testing_dtm <- testing_dtm/matrix(rep(apply(testing_dtm,1,sum),each = ncol(testing_dtm)), ncol = ncol(testing_dtm), by = TRUE)
apply(training_dtm, 1, sum)
apply(testing_dtm, 1, sum)


save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\nrowL1dtmBBCnews.Rdata")
timeend<-Sys.time()
timeend- timestart
