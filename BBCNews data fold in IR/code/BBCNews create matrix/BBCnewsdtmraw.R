#The dataset is the same as the dataset used in the first paper. In the first paper, we used five-fold cross validation; here we do not use five-fold cross validation

rm(list=ls())
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)
library(tm)
library(textstem)
library(caret)

minfre <- 10;

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}


# Define a function to read all files from a folder into a data frame
alldata_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 2/BBCNews data fold in IR/data/bbcnews/"

# Use unnest() and map() to apply read_folder to each subfolder
alldata_raw_text <- tibble(folder = dir(alldata_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)

alldata_usenet_words <- alldata_raw_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

alldata_usenet_words$word <- lemmatize_words(alldata_usenet_words$word)


# include only words that occur at least 50 times
alldata_word_newsgroups <- alldata_usenet_words %>%
  filter(str_detect(newsgroup, "")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()

# convert into a document-term matrix
# with document names such as sci.crypt_14147
alldata_dtm <- alldata_word_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

category <- gsub('[.txt]', '', rownames(as.matrix(alldata_dtm))) 
category <- gsub('[^a-z]', '', category)
category <- gsub('[_]', '', category)

set.seed(123)

train_idx <- createDataPartition(category, p=0.80, list=FALSE)

training_dtm <- as.matrix(alldata_dtm)[train_idx, ]
class(training_dtm)
testing_dtm <- as.matrix(alldata_dtm)[-train_idx, ]
class(testing_dtm)

# order_index <- order(-apply(training_dtm, 2, sum))
# training_dtm.terms <- rownames(as.matrix(apply(training_dtm, 2, sum)[order_index[1:mfi]]))
# training_dtm <- subset(training_dtm, select=c(training_dtm.terms))

training_dtm.terms <- colnames(training_dtm)[which(apply(training_dtm, 2, sum)>=minfre)]
training_dtm <- subset(training_dtm, select=c(training_dtm.terms))
dim(training_dtm)

dim(training_dtm)

testing_dtm.terms <- colnames(testing_dtm)
testing_dtm_row_names <- row.names(testing_dtm)

testing_new_matrix <- matrix(rep(NA,nrow(testing_dtm)*ncol(training_dtm)), nrow = nrow(testing_dtm), ncol = ncol(training_dtm))

for (j in 1:ncol(training_dtm)) {
  if (training_dtm.terms[j] %in% testing_dtm.terms) {
    testing_new_matrix[,j] <-  testing_dtm[, which(training_dtm.terms[j] == testing_dtm.terms)]
  } else {
    testing_new_matrix[,j] <-  0
  }
}
testing_dtm <- testing_new_matrix
row.names(testing_dtm) <- testing_dtm_row_names
colnames(testing_dtm) <- training_dtm.terms

print(which(apply(training_dtm, 1, sum) == 0))
print(which(apply(testing_dtm, 1, sum) == 0))


include_authors=c('business', 'entertainment', 'politics', 'sport', 'tech')

# training_dtm <- training_dtm[-which(apply(training_dtm, 1, sum) == 0), ]
# testing_dtm <- testing_dtm[-which(apply(testing_dtm, 1, sum) == 0), ]
# dim(training_dtm)
# dim(testing_dtm)

training_index <- row.names(training_dtm)

for (i in 1:length(training_index)){
  if (grepl('business', training_index[i])) {
    training_index[i] <- 1
  } else if (grepl('entertainment', training_index[i])) {
    training_index[i] <- 2
  } else if (grepl('politics', training_index[i])) {
    training_index[i] <- 3
  } else if (grepl('sport', training_index[i])) {
    training_index[i] <- 4
  } else if (grepl('tech', training_index[i])) {
    training_index[i] <- 5
  }
}

testing_index <- row.names(testing_dtm)
for (i in 1:length(testing_index)){
  if (grepl('business', testing_index[i])) {
    testing_index[i] <- 1
  } else if (grepl('entertainment', testing_index[i])) {
    testing_index[i] <- 2
  } else if (grepl('politics', testing_index[i])) {
    testing_index[i] <- 3
  } else if (grepl('sport', testing_index[i])) {
    testing_index[i] <- 4
  } else if (grepl('tech', testing_index[i])) {
    testing_index[i] <- 5
  }
}
print(dim(training_dtm))
print(dim(testing_dtm))


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


save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 2\\BBCNews data fold in IR\\created data\\created matrix\\rawdtmBBCnews.Rdata")


