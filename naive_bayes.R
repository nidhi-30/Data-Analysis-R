# load training dataset
train_dataset = read.csv("data_naive_train.csv")
train_df <- train_dataset
print(train_df)

# load testing dataset
test_dataset = read.csv("data_naive_test.csv")
test_df <- test_dataset
print(test_df)

# seperate the datapoints by target variable
p_t0 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 0,])/nrow(train_df)
print(p_t0)

p_t1 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 1,])/nrow(train_df)
print(p_t1)

P_c0_t0 <- c()
P_c0_t1 <- c()
P_c1_t0 <- c()
P_c1_t1 <- c()

# calculate m-estimate probabilities for each feature of training dataset
for(i in 1:(ncol(train_df)-1)){
  
  # get value of m (possible values of a feature)
  m <- length(unique(train_df[,i]))
  print(m)
  
  # get value of p (1/possible values of a feature)
  p <- 1/m
  print(p)
  
  # get number of rows to get value of nc
  nc_c0_t0 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 0 & train_df[,i] == 0,])
  print(nc_c0_t0)
  nc_c0_t1 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 1 & train_df[,i] == 0,])
  print(nc_c0_t1)
  nc_c1_t0 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 0 & train_df[,i] == 1,])
  print(nc_c1_t0)
  nc_c1_t1 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 1 & train_df[,i] == 1,])
  print(nc_c1_t1)
  
  # get number of rows to get value of n
  n_t0 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 0 ,])
  print(n_t0)
  n_t1 <- nrow(train_df[train_df$OVERALL_DIAGNOSIS == 1 ,])
  print(n_t1)
  
  # possible probabilities for selectes feature
  
  # probability if feature value is 0 and target variable is 0
  P_c0_t0[i] <- (nc_c0_t0 + (m*p)) / (n_t0 + m)
  # probability if feature value is 0 and target variable is 1
  P_c0_t1[i] <- (nc_c0_t1 + (m*p)) / (n_t1 + m)
  # probability if feature value is 1 and target variable is 0
  P_c1_t0[i] <- (nc_c1_t0 + (m*p)) / (n_t0 + m)
  # probability if feature value is 1 and target variable is 1
  P_c1_t1[i] <- (nc_c1_t1 + (m*p)) / (n_t1 + m)
  
}

# create probability matrix using probability values
probability_matrix <- data.frame(P_c0_t0,P_c0_t1,P_c1_t0,P_c1_t1)
print(probability_matrix)

# add new column to dataframe for predicted class values
test_df$Predicted_classes <- NA
print(test_df)

# use naive bayes algorithm to get class values of test dataset
probability_instance_0 <- c()
probability_instance_1 <- c()
for(i in 1:nrow(test_df)){
  for(j in 1:(ncol(test_df)-2)){
    feature_val <- test_df[i,j]

    # take probability value based on feature value
    if(feature_val == 0){
      probability_instance_0[j] <- probability_matrix[j,1]
      probability_instance_1[j] <- probability_matrix[j,2]
    }
    if(feature_val == 1){
      probability_instance_0[j] <- probability_matrix[j,3]
      probability_instance_1[j] <- probability_matrix[j,4]
    }
    
  }
  
  probability_row_0 <- 1
  probability_row_1 <- 1
  
  # multiply probabilities values based on feature value to get target variable
  for(k in 1:length(probability_instance_0)){
    probability_row_0 <- probability_row_0 * probability_instance_0[k]
    probability_row_1 <- probability_row_1 * probability_instance_1[k]
  }
  
  # multiply probability values based on feature value to get target variable
  probability_row_0 <- probability_row_0 * p_t0
  probability_row_1 <- probability_row_1 * p_t1
  
  # assign class variable based on values of probilities (class with highest probability will be assigned as class variable)
  if(probability_row_0 > probability_row_1){
    test_df[i,ncol(test_df)] <- 0
  }
  if(probability_row_0 < probability_row_1){
    test_df[i,ncol(test_df)] <- 1
  }
  
}
print(test_df)

# write csv file which contain new feature named predicted class
write.csv(test_df, file = "predictions.csv")

# calculate confusion matrix
# 1 as positive and 0 as negative
predicted_0 <- c(0,0)
predicted_1 <- c(0,0)

# compare value of target class and predicted class on testing set
confusion_matrix <- data.frame(predicted_1,predicted_0)
row_names <- c("original_1","original_0")
row.names(confusion_matrix) <- row_names
print(confusion_matrix)

# increase value by 1 based on values of target class and predicted class for each datapoints
for(i in 1:nrow(test_df)){
  original_value <- test_df[i,(ncol(test_df)-1)]
  predicted_value <- test_df[i,(ncol(test_df))]
  
  if(original_value == 0 && predicted_value == 0)
    confusion_matrix[2,2] <- confusion_matrix[2,2]+1
  
  if(original_value == 0 && predicted_value == 1)
    confusion_matrix[2,1] <- confusion_matrix[2,1]+1
  
  if(original_value == 1 && predicted_value == 0)
    confusion_matrix[1,2] <- confusion_matrix[1,2]+1
  
  if(original_value == 1 && predicted_value == 1)
    confusion_matrix[1,1] <- confusion_matrix[1,1]+1
  
}

print(confusion_matrix)

# calculate accuracy : (TP+TN)/ (TP+FP+TN+FN)
accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/(confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]+confusion_matrix[2,2])
print(accuracy)

# calculate sensitivity : TP/(TP+FN)
sensitivity <- (confusion_matrix[1,1])/(confusion_matrix[1,1]+confusion_matrix[1,2])
print(sensitivity)

# calculate specificity : TN/(TN+FP)
specificity <- (confusion_matrix[2,2])/(confusion_matrix[2,2]+confusion_matrix[2,1])
print(specificity)