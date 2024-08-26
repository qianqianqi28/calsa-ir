# information retrieval
meanprerawnrowtfidfpvalue <- function (dtm.training.svd.ud, dtm.testing.svd.ud, training_index, testing_index, metric = "euclidean", retrieved_document_number = 20, recall_value) {
  nrow_testing <- nrow(dtm.testing.svd.ud)

  Euc_ap_total <- matrix(NA,nrow = nrow_testing,ncol=1, byrow=TRUE)
  Cos_ap_total <- matrix(NA,nrow = nrow_testing,ncol=1, byrow=TRUE)
  Dot_ap_total <- matrix(NA,nrow = nrow_testing,ncol=1, byrow=TRUE)
  
  #Euclidean
  
  Euc_test_train <- apply(dtm.training.svd.ud,1,function(y) apply(dtm.testing.svd.ud,1,function(x,y)dist(rbind(x,y)),y))

  
  #Cosine
 
  Cos_test_train <- apply(dtm.training.svd.ud,1,function(y) apply(dtm.testing.svd.ud,1,function(x,y)cosine(x,y),y))

  
  #Dot
  
  Dot_test_train <- apply(as.matrix(dtm.training.svd.ud),1,function(y) apply(as.matrix(dtm.testing.svd.ud),1,function(x,y)(t(x) %*% y),y))
  
  for(i in 1:nrow_testing){
    
    #Euclidean
    
    Euc_query_train_one <- as.matrix(Euc_test_train[i,])
    Euc_query_train_sort <- as.matrix(sort(Euc_query_train_one[,1]))
    
    Euc_tp_false_true <- rownames(Euc_query_train_sort)[1:retrieved_document_number]  == testing_index[i]
    Euc_tp_cumsum <- t(as.matrix(cumsum(Euc_tp_false_true)))
    Euc_prec <- Euc_tp_cumsum/seq(1:retrieved_document_number)
    Euc_rec <- Euc_tp_cumsum/recall_value[1,i]
    
    ap <- 0
    m =0
    for (m in seq(from=0, to=1, by=0.1)){
      if (sum(Euc_rec[1,] >= m) == 0) {
        p = 0
      } else {
        p = max(Euc_prec[1, Euc_rec[1,] >= m])
      }
      ap = ap+p/11
    }
    Euc_ap_total[i,1] <- ap
    
    # Cosine
    
    Cos_query_train_one <- as.matrix(Cos_test_train[i,])
    Cos_query_train_sort <- as.matrix(sort(Cos_query_train_one[,1], decreasing = TRUE))
    
    Cos_tp_false_true <- rownames(Cos_query_train_sort)[1:retrieved_document_number]  == testing_index[i]
    Cos_tp_cumsum <- t(as.matrix(cumsum(Cos_tp_false_true)))
    Cos_prec <- Cos_tp_cumsum/seq(1:retrieved_document_number)
    Cos_rec <- Cos_tp_cumsum/recall_value[1,i]
    
    ap <- 0
    m =0
    for (m in seq(from=0, to=1, by=0.1)){
      if (sum(Cos_rec[1,] >= m) == 0) {
        p = 0
      } else {
        p = max(Cos_prec[1, Cos_rec[1,] >= m])
      }
      ap = ap+p/11
    }
    Cos_ap_total[i,1] <- ap
    
    
    # Dot
    
    Dot_query_train_one <- as.matrix(Dot_test_train[i,])
    Dot_query_train_sort <- as.matrix(sort(Dot_query_train_one[,1], decreasing = TRUE))
    
    Dot_tp_false_true <- rownames(Dot_query_train_sort)[1:retrieved_document_number]  == testing_index[i]
    Dot_tp_cumsum <- t(as.matrix(cumsum(Dot_tp_false_true)))
    Dot_prec <- Dot_tp_cumsum/seq(1:retrieved_document_number)
    Dot_rec <- Dot_tp_cumsum/recall_value[1,i]
    
    ap <- 0
    m =0
    for (m in seq(from=0, to=1, by=0.1)){
      if (sum(Dot_rec[1,] >= m) == 0) {
        p = 0
      } else {
        p = max(Dot_prec[1, Dot_rec[1,] >= m])
      }
      ap = ap+p/11
    }
    Dot_ap_total[i,1] <- ap
    
  }
  

  Euc_maprecision <- apply(Euc_ap_total, 2, sum)/nrow_testing
  Cos_maprecision <- apply(Cos_ap_total, 2, sum)/nrow_testing
  Dot_maprecision <- apply(Dot_ap_total, 2, sum)/nrow_testing
  
  maprecision <- list(Euc_maprecision,Cos_maprecision,Dot_maprecision)
  
  return(maprecision)
}

