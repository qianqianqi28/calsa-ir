#For the second paper: We randomly choose 600 documents from the training set of four categories (comp.graphics, rec.sport.hockey, sci.crypt, and talk.politics.guns) and 400 documents from the test set of these four categories.


rm(list=ls())
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\20Newsgroups fold in IR\\created data\\created matrix\\rawdtm20newsgroups.Rdata")
dim(training_dtm)
training_dtm <- training_dtm/matrix(rep(sqrt(apply(training_dtm^2,1,sum)),each = ncol(training_dtm)), ncol = ncol(training_dtm), by = TRUE)
testing_dtm <- testing_dtm/matrix(rep(sqrt(apply(testing_dtm^2,1,sum)),each = ncol(testing_dtm)), ncol = ncol(testing_dtm), by = TRUE)
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\20Newsgroups fold in IR\\created data\\created matrix\\nrowL2dtm20newsgroups.Rdata")


