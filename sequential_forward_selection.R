#import required libraries
library(e1071)
library(DMwR)

#load the dataset
dataset = read.csv("data_feature_selection.csv")
df <- dataset
print(df)
df[sapply(df, is.factor)] <- data.matrix(df[sapply(df, is.factor)])

# get feature with class variable
df_class <- df[,ncol(df)]

# scale the dataset
df <- as.data.frame(scale(df[1:(ncol(df)-1)]))
df$Class <- df_class

# impute missing values in dataset using knn imputation
df <- knnImputation(df)
print(df)

# sepearate datapoints using class values
dt_2 <- df[df$Class == 2,]
strata_2 <- nrow(dt_2)
print(strata_2)
dt_1 <- df[df$Class == 1,]
strata_1 <- nrow(dt_1)
print(strata_1)

# divide number of rows by 5 to get same proportion in each fold
target_2_fold <- as.integer(strata_2/5)
target_1_fold <- as.integer(strata_1/5)

# get remained rows after dividing rows by 5 for each class value
remained_rows_2 <- strata_2 - (target_2_fold * 5)
print(remained_rows_2)
remained_rows_1 <- strata_1 - (target_1_fold * 5)
print(remained_rows_1)

#set.seed(1234)

# randomly select number if remained rows in class values
if(remained_rows_1 > 0){
  random_sample_1 <- c(1:5)
  random_number_1 <- sample(random_sample_1,remained_rows_1)
  print(random_number_1)
}
if(remained_rows_2 > 0){
  random_sample_2 <- c(1:5)
  random_number_2 <- sample(random_sample_2,remained_rows_2)
  print(random_number_2)
}

# get 5 different folds using datapoints for stratified 5-fold cross-validation
for(i in 1:5){
  print("TO BE REMOVED")
  print(i)
  
  # randomly select sample without replacement based on size of each class value to maintain same proportion in each field
  if(i %in% random_number_1){
    fold_n_t2 <- dt_2[sample(nrow(dt_2), (target_2_fold ), replace = FALSE),]
    print(fold_n_t2)
    
    fold_n_t1 <- dt_1[sample(nrow(dt_1), (target_1_fold + 1), replace = FALSE),]
    print(fold_n_t1)
  }
  if(i %in% random_number_2){
    fold_n_t2 <- dt_2[sample(nrow(dt_2), (target_2_fold + 1), replace = FALSE),]
    print(fold_n_t2)
    
    fold_n_t1 <- dt_1[sample(nrow(dt_1), (target_1_fold), replace = FALSE),]
    print(fold_n_t1)
  }
  else{
    fold_n_t2 <- dt_2[sample(nrow(dt_2), target_2_fold, replace = FALSE),]
    print(fold_n_t2)
    
    fold_n_t1 <- dt_1[sample(nrow(dt_1), target_1_fold, replace = FALSE),]
    print(fold_n_t1)
  }
  
  # sort indexes of each class values for each folds
  rm_rows_2 <- sort(as.numeric(rownames(fold_n_t2)),decreasing = FALSE)
  #print(rm_rows_2)
  rm_rows_1 <- sort(as.numeric(rownames(fold_n_t1)),decreasing = FALSE)
  #print(rm_rows_1)
  
  # remove selected datapoints so that it will not be selected in next fold
  dt_2 <- dt_2[!(row.names(dt_2) %in% rm_rows_2),]
  print(dt_2)
  dt_1 <- dt_1[!(row.names(dt_1) %in% rm_rows_1),]
  print(dt_1)
  
  # assign indexes by merging indexes of class values for each fold
  nam <- paste("fold", i, sep = "_")
  assign(nam, append(rm_rows_1,rm_rows_2))
  
}

# print all five folds after startified 5-fold cross validation
print(fold_1)
print(fold_2)
print(fold_3)
print(fold_4)
print(fold_5)

# create feature set with no features in it
feature_set <- c()
feature_set_index <- c()

# get full feature set which contains all the features that are used to predict class variable
full_feture_set <- colnames(df[,1:(ncol(df)-1)])
print(full_feture_set)

acc_max_feature <- 0
prev_accuracy <- 0
max_feature <- 0
index <- 1
remove_index <- 0

while(index < ncol(df)){
  
  # check accuray for each feature
  for(i in 1:length(full_feture_set)){
    # if feature set is empty then check accuracy for one feature
    if(length(feature_set) == 0){
      vect <- c(i,ncol(df))
      dt <- df[,vect]
    }
    # if feature set is not empty then check accuracy for feature in feature set and its combination with each remained features
    else{
      vect <- c(feature_set_index,i,ncol(df))
      dt <- df[,vect]
    }
    
    accuracy <- 0
    
    # get accuracy for each folds
    for(j in 1:5){
      
      print(j)
      fold_name <- paste0("fold_", j)
      
      fold_j <- get(fold_name)
      print(fold_j)
      
      # take training and testing sets based on values of each folds
      test_df <- dt[fold_j,]
      train_df <- dt[-(fold_j),]
      
      # apply svm learning algorithm to calculate accuracy
      classifier <- svm(formula = Class ~ ., data = train_df, type = 'C-classification', kernel = 'linear')
      print(classifier)
      predict_test <- predict(classifier, newdata = test_df)
      print(predict_test)
      
      # add accuracy after each fold
      accuracy <- accuracy + (mean( predict_test == test_df$Class))
    }
    
    # divide accuracy by 5 to get average accuracy for all folds
    accuracy <- accuracy/5
    
    # get maximum accuracy to get important feature in feature set
    if(acc_max_feature < accuracy){
      acc_max_feature <- accuracy
      max_feature <- full_feture_set[i]
      remove_index <- i
      print(max_feature)
    }
    
  }
  
  # for maximum accuracy add feature to feature set
  if(prev_accuracy < acc_max_feature){
    prev_accuracy <- acc_max_feature
    
    # add feature to the feature set
    feature_set[index] <- max_feature
    feature_set_index[index] <- remove_index
    index <- index+1
    print(max_feature)
    print(acc_max_feature)
    
    # remove important feature from the full feature set
    full_feture_set <- full_feture_set[-(remove_index)]
    print(full_feture_set)
  }
  
  # if accuracy does not improve break the loop
  else{
    break
  }
  
}

print(feature_set)