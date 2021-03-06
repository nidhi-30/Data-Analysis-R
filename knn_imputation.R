#load the dataset
dataset=read.csv("data.csv")
df <- dataset
df[sapply(df, is.factor)] <- data.matrix(df[sapply(df, is.factor)])
print(df)
summary(df)

#columns for database
Method <- c('5%_1NN_EUC','5%_1NN_MAN','5%_5NN_EUC','5%_5NN_MAN','5%_w5NN_EUC','5%_w5NN_MAN','10%_1NN_EUC','10%_1NN_MAN','10%_5NN_EUC','10%_5NN_MAN','10%_w5NN_EUC','10%_w5NN_MAN','20%_1NN_EUC','20%_1NN_MAN','20%_5NN_EUC','20%_5NN_MAN','20%_w5NN_EUC','20%_w5NN_MAN','5%_1NN_EUC_FS1','5%_1NN_MAN_FS1','5%_5NN_EUC_FS1','5%_5NN_MAN_FS1','5%_w5NN_EUC_FS1','5%_w5NN_MAN_FS1','10%_1NN_EUC_FS1','10%_1NN_MAN_FS1','10%_5NN_EUC_FS1','10%_5NN_MAN_FS1','10%_w5NN_EUC_FS1','10%_w5NN_MAN_FS1','20%_1NN_EUC_FS1','20%_1NN_MAN_FS1','20%_5NN_EUC_FS1','20%_5NN_MAN_Fs1','20%_w5NN_EUC_FS1','20%_w5NN_MAN_FS1','5%_1NN_EUC_FS2','5%_1NN_MAN_FS2','5%_5NN_EUC_FS2','5%_5NN_MAN_FS2','5%_w5NN_EUC_FS2','5%_w5NN_MAN_FS2','10%_1NN_EUC_FS2','10%_1NN_MAN_FS2','10%_5NN_EUC_FS2','10%_5NN_MAN_FS2','10%_w5NN_EUC_FS2','10%_w5NN_MAN_FS2','20%_1NN_EUC_FS2','20%_1NN_MAN_FS2','20%_5NN_EUC_FS2','20%_5NN_MAN_Fs2','20%_w5NN_EUC_FS2','20%_w5NN_MAN_FS2')
Accuracy_Age <- c()
Accuracy_Income <- c()
Accuracy_Gender <- c()
Accuracy_Illness <- c()

ans <- c()
index_5nn_euc <- c()
index_w5nn_euc <- c()
index_5nn_man <- c()
index_w5nn_man <- c()
val_index_mode <- c()

#function for mode to get frequency of data
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#function for normalizing data using min-max
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#create missingness and impute values
for(percentage in 1:3){
  for(column in 3:ncol(df)){
    
    df_euc_1nn <- df
    
    #remove data randomly (5%, 10% and 20%)
    if(percentage == 1)
      df_euc_1nn[sample(1:NROW(df),0.05*NROW(df)),column] <- NA
    else if(percentage == 2)
      df_euc_1nn[sample(1:NROW(df),0.1*NROW(
	  df)),column] <- NA
    else
      df_euc_1nn[sample(1:NROW(df),0.2*NROW(df)),column] <- NA
    summary(df_euc_1nn)
    
    
    df_man_1nn <- df_euc_1nn
    df_euc_5nn <- df_euc_1nn
    df_man_5nn <- df_euc_1nn
    df_euc_w5nn <- df_euc_1nn
    df_man_w5nn <- df_euc_1nn
    
    name_column <- colnames(df_euc_1nn,prefix="col")[column]
  
    # seperate instances with NA and without NA by 
    input <- df_euc_1nn[!(is.na(df_euc_1nn[[column]])),]
    y <- df_euc_1nn[is.na(df_euc_1nn[[column]]),]

    #K-nn for K=1, count distance (euclidian distance)
    total_euc_1nn <- 0
    error_euc_1nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_1nn) %in% name_column)]
      print(x1)
      #get original value at index
      val_index <- df[df$Number == x1$Number ,name_column]
      print(val_index)
      #add original values
      total_euc_1nn <- sum(total_euc_1nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_1nn) %in% name_column)]
        print(x2)

        #calculate distance
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        
        #add distance to a vector
        index <- index+1
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        # impute_val[i] <- min(ans)
      }
      #find minimum distance
      print(ans)
      impute_val_dist <- min(ans)

      #find index of minimum distance
      impute_val_index <- which(ans == impute_val_dist)
      print(impute_val_index)

      #get predicted value from the index and added to dataset
      impute_val <- input[input$Number == impute_val_index, name_column]
      df_euc_1nn[df_euc_1nn$Number == x1$Number ,name_column] <- impute_val
      print(impute_val)

      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_1nn <- sum(error_euc_1nn,abs(impute_val - val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_1nn <- (1-(error_euc_1nn/total_euc_1nn))*100
    print(accuracy_euc_1nn)
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_1nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_1nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_1nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_1nn)
    }
    
    #K-nn for K=1, count distance (manhattan distance)
    total_man_1nn <- 0
    error_man_1nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_1nn) %in% name_column)]
      #get original value at index
      val_index <- df[df$Number == x1$Number ,name_column]
      #add original values
      total_man_1nn <- sum(total_man_1nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_1nn) %in% name_column)]

        #add distance to a vector
        index <- index+1
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        #calculate distance
        ans[index] <- sum(abs(x1 - x2))
      }
      
      #find minimum distance
      impute_val_dist <- min(ans)

      #find index of minimum distance
      impute_val_index <- which(ans == impute_val_dist)

      #get predicted value from the index and added to dataset
      impute_val <- input[input$Number == impute_val_index, name_column]
      df_man_1nn[df_man_1nn$Number == x1$Number ,name_column] <- impute_val

      #find error in data by subtracting original value from dataframe and predicted value
      error_man_1nn <- sum(error_man_1nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_1nn <- (1-(error_man_1nn/total_man_1nn))*100

    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_1nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_1nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_1nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_1nn)
    }
    
    
    #K-nn for K=5, count distance (euclidian distance)
    total_euc_5nn <- 0
    error_euc_5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_5nn) %in% name_column)]

      #get original value at index
      val_index <- df[df$Number == x1$Number ,name_column]
      #add original values
      total_euc_5nn <- sum(total_euc_5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_5nn) %in% name_column)]

        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        index <- index+1
      }
      #find minimum five distance
      sort_ans_euc <- sort(unique(ans),decreasing = F) [1:5]
      sum_5nn_euc <- 0
      for(k in 1:5){
        if(column == 3 || column == 6){
          index_5nn_euc[k] <- which(sort_ans_euc[k] == ans)
          val_index_mode[k] <- input[input$Number == index_5nn_euc[k], name_column]
        }
        else{
          index_5nn_euc[k] <- which(sort_ans_euc[k] == ans)
          val_at_index <- input[input$Number == index_5nn_euc[k], name_column]
          sum_5nn_euc <- sum(sum_5nn_euc,val_at_index)
        }
      }
      # calculate average for continuous feature and mode for categorical data
      if(column == 3 || column == 6){
        impute_val <- getmode(val_index_mode)
      }
      else{
        impute_val <- round(sum_5nn_euc / 5)
      }
      #get predicted value from the index and added to dataset
      df_euc_5nn[df_euc_5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_5nn <- sum(error_euc_5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_5nn <- (1-(error_euc_5nn/total_euc_5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_5nn)
    }
    
    #K-nn for K=5,count distance (manhattan distance)
    total_man_5nn <- 0
    error_man_5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_5nn) %in% name_column)]
      #get original value at index
      val_index <- df[df$Number == x1$Number ,name_column]
      #add original values
      total_man_5nn <- sum(total_man_5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_5nn) %in% name_column)]

        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sum(abs(x1 - x2))
        index <- index+1
      }
      sort_ans_man <- sort(ans,decreasing = F) [1:5]
      sum_5nn_man <- 0
      #find minimum  five distances
      for(k in 1:5){
        if(column == 3 || column == 6){
          index_5nn_man[k] <- which(sort_ans_man[k] == ans)
          val_index_mode[k] <- input[input$Number == index_5nn_man[k], name_column]
        }
        else{
          index_5nn_man[k] <- which(sort_ans_man[k] == ans)
          val_at_index <- input[input$Number == index_5nn_man[k], name_column]
          sum_5nn_man <- sum(sum_5nn_man,val_at_index)
        }
      }
      # calculate average for continuous feature and mode for categorical data
      if(column == 3 || column == 6){
        getmode(val_index_mode)
      }
      else{
        impute_val <- round(sum_5nn_man / 5)
      }
      #get predicted value from the index and added to dataset
      df_man_5nn[df_man_5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_5nn <- sum(error_man_5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_5nn <- (1-(error_man_5nn/total_man_5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_5nn)
    }
    
    
    #Weighted K-nn for K=5, count distance (euclidian distance)
    total_euc_w5nn <- 0
    error_euc_w5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_w5nn) %in% name_column)]
      #get original value at index
      val_index <- df[df$Number == x1$Number ,name_column]
      #add original values
      total_euc_w5nn <- sum(total_euc_w5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_w5nn) %in% name_column)]

        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        index <- index+1
      }
      #find minimum five distances
      sort_ans_euc <- sort(ans,decreasing = F) [1:5]
      sum_w5nn <- 0
      sum_weight_wnn <- 0 
      for(k in 1:5){
        weight_val_euc <- 1 / sort_ans_euc[k]
        index_w5nn_euc[k] <- which(sort_ans_euc[k] == ans)
        val_at_index <- input[input$Number == index_w5nn_euc[k], name_column]
        sum_w5nn <- sum(sum_w5nn,(val_at_index*weight_val_euc))
        sum_weight_wnn <- sum(sum_weight_wnn,weight_val_euc)
      }
      #get predicted value from the index and added to dataset
      impute_val <- round(sum_w5nn / sum_weight_wnn)
      df_euc_w5nn[df_euc_w5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_w5nn <- sum(error_euc_w5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_w5nn <- (1-(error_euc_w5nn/total_euc_w5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_w5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_w5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_w5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_w5nn)
    }
    
    #Weighted K-nn for K=5, count distance (manhattan distance)
    total_man_w5nn <- 0
    error_man_w5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_w5nn) %in% name_column)]
      val_index <- df[df$Number == x1$Number ,name_column]
      #add original values
      total_man_w5nn <- sum(total_man_w5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_w5nn) %in% name_column)]

        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sum(abs(x1 - x2))
        index <- index+1
      }
      #find minimum five distances
      sort_ans_man <- sort(ans,decreasing = F) [1:5]
      sum_w5nn <- 0
      sum_weight_wnn <- 0
      for(k in 1:5){
        weight_val_man <- 1 / sort_ans_man[k]
        index_w5nn_man[k] <- which(sort_ans_man[k] == ans)
        val_at_index <- input[input$Number == index_w5nn_man[k], name_column]
        print(val_at_index)
        sum_w5nn <- sum(sum_w5nn,(val_at_index*weight_val_man))
        sum_weight_wnn <- sum(sum_weight_wnn,weight_val_man)
      }
      #get predicted value from the index and added to dataset
      impute_val <- round(sum_w5nn / sum_weight_wnn)
      df_man_w5nn[df_man_w5nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_w5nn <- sum(error_man_w5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_w5nn <- (1-(error_man_w5nn/total_man_w5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_w5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_w5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_w5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_w5nn)
    }
  
  }
  
}


#feature scaling using min-max
df_norm_1 <- as.data.frame(lapply(df[3:6], normalize))
df_norm_1[,"Name"] <- 1:nrow(df)

#create missingness and impute values
for(percentage in 1:3){
  for(column in 3:ncol(df_norm_1)){
    
    df_euc_1nn <- df_norm_1
    
    #remove data randomly (5%, 10% and 20%)
    if(percentage == 1)
      df_euc_1nn[sample(1:NROW(df_norm_1),0.05*NROW(df_norm_1)),column] <- NA
    else if(percentage == 2)
      df_euc_1nn[sample(1:NROW(df_norm_1),0.1*NROW(df_norm_1)),column] <- NA
    else
      df_euc_1nn[sample(1:NROW(df_norm_1),0.2*NROW(df_norm_1)),column] <- NA
    summary(df_euc_1nn)
    
    
    df_man_1nn <- df_euc_1nn
    df_euc_5nn <- df_euc_1nn
    df_man_5nn <- df_euc_1nn
    df_euc_w5nn <- df_euc_1nn
    df_man_w5nn <- df_euc_1nn
    
    name_column <- colnames(df_euc_1nn,prefix="col")[column]
    
    # seperate instances with NA and without NA by 
    input <- df_euc_1nn[!(is.na(df_euc_1nn[[column]])),]
    y <- df_euc_1nn[is.na(df_euc_1nn[[column]]),]
    
    #K-nn for K=1, count distance (euclidian distance)
    total_euc_1nn <- 0
    error_euc_1nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_1nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_1[df_norm_1$Number == x1$Number ,name_column]
      #add original values
      total_euc_1nn <- sum(total_euc_1nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_1nn) %in% name_column)]
        
        #calculate distance
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        
        #add distance to a vector
        index <- index+1
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        # impute_val[i] <- min(ans)
      }
      #find minimum distance
      print(ans)
      impute_val_dist <- min(ans)
      
      #find index of minimum distance
      impute_val_index <- which(ans == impute_val_dist)
      
      #get predicted value from the index and added to dataset
      impute_val <- input[input$Number == impute_val_index, name_column]
      df_euc_1nn[df_euc_1nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_1nn <- sum(error_euc_1nn,abs(impute_val - val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_1nn <- (1-(error_euc_1nn/total_euc_1nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_1nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_1nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_1nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_1nn)
    }
    
    #K-nn for K=1, count distance (manhattan distance)
    total_man_1nn <- 0
    error_man_1nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_1nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_1[df_norm_1$Number == x1$Number ,name_column]
      #add original values
      total_man_1nn <- sum(total_man_1nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_1nn) %in% name_column)]
        
        #add distance to a vector
        index <- index+1
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        #calculate distance
        ans[index] <- sum(abs(x1 - x2))
      }
      
      #find minimum distance
      impute_val_dist <- min(ans)
      
      #find index of minimum distance
      impute_val_index <- which(ans == impute_val_dist)
      
      #get predicted value from the index and added to dataset
      impute_val <- input[input$Number == impute_val_index, name_column]
      df_man_1nn[df_man_1nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_1nn <- sum(error_man_1nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_1nn <- (1-(error_man_1nn/total_man_1nn))*100
    
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_1nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_1nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_1nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_1nn)
    }
    
    
    #K-nn for K=5, count distance (euclidian distance)
    total_euc_5nn <- 0
    error_euc_5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_5nn) %in% name_column)]
      
      #get original value at index
      val_index <- df_norm_1[df_norm_1$Number == x1$Number ,name_column]
      #add original values
      total_euc_5nn <- sum(total_euc_5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        index <- index+1
      }
      #find minimum five distance
      sort_ans_euc <- sort(unique(ans),decreasing = F) [1:5]
      sum_5nn_euc <- 0
      for(k in 1:5){
        if(column == 3 || column == 6){
          index_5nn_euc[k] <- which(sort_ans_euc[k] == ans)
          val_index_mode[k] <- input[input$Number == index_5nn_euc[k], name_column]
        }
        else{
          index_5nn_euc[k] <- which(sort_ans_euc[k] == ans)
          val_at_index <- input[input$Number == index_5nn_euc[k], name_column]
          sum_5nn_euc <- sum(sum_5nn_euc,val_at_index)
        }
      }
      # calculate average for continuous feature and mode for categorical data
      if(column == 3 || column == 6){
        getmode(val_index_mode)
      }
      else{
        impute_val <- round(sum_5nn_euc / 5)
      }
      #get predicted value from the index and added to dataset
      df_euc_5nn[df_euc_5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_5nn <- sum(error_euc_5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_5nn <- (1-(error_euc_5nn/total_euc_5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_5nn)
    }
    
    #K-nn for K=5,count distance (manhattan distance)
    total_man_5nn <- 0
    error_man_5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_5nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_1[df_norm_1$Number == x1$Number ,name_column]
      #add original values
      total_man_5nn <- sum(total_man_5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sum(abs(x1 - x2))
        index <- index+1
      }
      sort_ans_man <- sort(ans,decreasing = F) [1:5]
      sum_5nn_man <- 0
      #find minimum  five distances
      for(k in 1:5){
        if(column == 3 || column == 6){
          index_5nn_man[k] <- which(sort_ans_man[k] == ans)
          val_index_mode[k] <- input[input$Number == index_5nn_man[k], name_column]
        }
        else{
          index_5nn_man[k] <- which(sort_ans_man[k] == ans)
          val_at_index <- input[input$Number == index_5nn_man[k], name_column]
          sum_5nn_man <- sum(sum_5nn_man,val_at_index)
        }
      }
      # calculate average for continuous feature and mode for categorical data
      if(column == 3 || column == 6){
        getmode(val_index_mode)
      }
      else{
        impute_val <- round(sum_5nn_man / 5)
      }
      #get predicted value from the index and added to dataset
      df_man_5nn[df_man_5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_5nn <- sum(error_man_5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_5nn <- (1-(error_man_5nn/total_man_5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_5nn)
    }
    
    
    #Weighted K-nn for K=5, count distance (euclidian distance)
    total_euc_w5nn <- 0
    error_euc_w5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_w5nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_1[df_norm_1$Number == x1$Number ,name_column]
      #add original values
      total_euc_w5nn <- sum(total_euc_w5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_w5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        index <- index+1
      }
      #find minimum five distances
      sort_ans_euc <- sort(ans,decreasing = F) [1:5]
      sum_w5nn <- 0
      sum_weight_wnn <- 0 
      for(k in 1:5){
        weight_val_euc <- 1 / sort_ans_euc[k]
        index_w5nn_euc[k] <- which(sort_ans_euc[k] == ans)
        val_at_index <- input[input$Number == index_w5nn_euc[k], name_column]
        sum_w5nn <- sum(sum_w5nn,(val_at_index*weight_val_euc))
        sum_weight_wnn <- sum(sum_weight_wnn,weight_val_euc)
      }
      #get predicted value from the index and added to dataset
      impute_val <- round(sum_w5nn / sum_weight_wnn)
      df_euc_w5nn[df_euc_w5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_w5nn <- sum(error_euc_w5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_w5nn <- (1-(error_euc_w5nn/total_euc_w5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_w5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_w5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_w5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_w5nn)
    }
    
    #Weighted K-nn for K=5, count distance (manhattan distance)
    total_man_w5nn <- 0
    error_man_w5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_w5nn) %in% name_column)]
      val_index <- df_norm_1[df_norm_1$Number == x1$Number ,name_column]
      #add original values
      total_man_w5nn <- sum(total_man_w5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_w5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sum(abs(x1 - x2))
        index <- index+1
      }
      #find minimum five distances
      sort_ans_man <- sort(ans,decreasing = F) [1:5]
      sum_w5nn <- 0
      sum_weight_wnn <- 0
      for(k in 1:5){
        weight_val_man <- 1 / sort_ans_man[k]
        index_w5nn_man[k] <- which(sort_ans_man[k] == ans)
        val_at_index <- input[input$Number == index_w5nn_man[k], name_column]
        print(val_at_index)
        sum_w5nn <- sum(sum_w5nn,(val_at_index*weight_val_man))
        sum_weight_wnn <- sum(sum_weight_wnn,weight_val_man)
      }
      #get predicted value from the index and added to dataset
      impute_val <- round(sum_w5nn / sum_weight_wnn)
      df_man_w5nn[df_man_w5nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_w5nn <- sum(error_man_w5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_w5nn <- (1-(error_man_w5nn/total_man_w5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_w5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_w5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_w5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_w5nn)
    }
    
  }
  
}

#feature scaling using z-score
df_norm_2 <- as.data.frame( scale(df[3:6]))
df_norm_2[,"Name"] <- 1:nrow(df)

#create missingness and impute values
for(percentage in 1:3){
  for(column in 3:ncol(df_norm_2)){
    
    df_euc_1nn <- df_norm_2
    
    #remove data randomly (5%, 10% and 20%)
    if(percentage == 1)
      df_euc_1nn[sample(1:NROW(df_norm_2),0.05*NROW(df_norm_2)),column] <- NA
    else if(percentage == 2)
      df_euc_1nn[sample(1:NROW(df_norm_2),0.1*NROW(df_norm_2)),column] <- NA
    else
      df_euc_1nn[sample(1:NROW(df_norm_2),0.2*NROW(df_norm_2)),column] <- NA
    summary(df_euc_1nn)
    
    
    df_man_1nn <- df_euc_1nn
    df_euc_5nn <- df_euc_1nn
    df_man_5nn <- df_euc_1nn
    df_euc_w5nn <- df_euc_1nn
    df_man_w5nn <- df_euc_1nn
    
    name_column <- colnames(df_euc_1nn,prefix="col")[column]
    
    # seperate instances with NA and without NA by 
    input <- df_euc_1nn[!(is.na(df_euc_1nn[[column]])),]
    y <- df_euc_1nn[is.na(df_euc_1nn[[column]]),]
    
    #K-nn for K=1, count distance (euclidian distance)
    total_euc_1nn <- 0
    error_euc_1nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_1nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_2[df_norm_2$Number == x1$Number ,name_column]
      #add original values
      total_euc_1nn <- sum(total_euc_1nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_1nn) %in% name_column)]
        
        #calculate distance
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        
        #add distance to a vector
        index <- index+1
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        # impute_val[i] <- min(ans)
      }
      #find minimum distance
      impute_val_dist <- min(ans)
      
      #find index of minimum distance
      impute_val_index <- which(ans == impute_val_dist)
      
      #get predicted value from the index and added to dataset
      impute_val <- input[input$Number == impute_val_index, name_column]
      df_euc_1nn[df_euc_1nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_1nn <- sum(error_euc_1nn,abs(impute_val - val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_1nn <- (1-(error_euc_1nn/total_euc_1nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_1nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_1nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_1nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_1nn)
    }
    
    #K-nn for K=1, count distance (manhattan distance)
    total_man_1nn <- 0
    error_man_1nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_1nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_2[df_norm_2$Number == x1$Number ,name_column]
      #add original values
      total_man_1nn <- sum(total_man_1nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_1nn) %in% name_column)]
        
        #add distance to a vector
        index <- index+1
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        #calculate distance
        ans[index] <- sum(abs(x1 - x2))
      }
      
      #find minimum distance
      impute_val_dist <- min(ans)
      
      #find index of minimum distance
      impute_val_index <- which(ans == impute_val_dist)
      
      #get predicted value from the index and added to dataset
      impute_val <- input[input$Number == impute_val_index, name_column]
      df_man_1nn[df_man_1nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_1nn <- sum(error_man_1nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_1nn <- (1-(error_man_1nn/total_man_1nn))*100
    
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_1nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_1nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_1nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_1nn)
    }
    
    
    #K-nn for K=5, count distance (euclidian distance)
    total_euc_5nn <- 0
    error_euc_5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_5nn) %in% name_column)]
      
      #get original value at index
      val_index <- df_norm_2[df_norm_2$Number == x1$Number ,name_column]
      #add original values
      total_euc_5nn <- sum(total_euc_5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        index <- index+1
      }
      #find minimum five distance
      sort_ans_euc <- sort(unique(ans),decreasing = F) [1:5]
      sum_5nn_euc <- 0
      for(k in 1:5){
        if(column == 3 || column == 6){
          index_5nn_euc[k] <- which(sort_ans_euc[k] == ans)
          val_index_mode[k] <- input[input$Number == index_5nn_euc[k], name_column]
        }
        else{
          index_5nn_euc[k] <- which(sort_ans_euc[k] == ans)
          val_at_index <- input[input$Number == index_5nn_euc[k], name_column]
          sum_5nn_euc <- sum(sum_5nn_euc,val_at_index)
        }
      }
      # calculate average for continuous feature and mode for categorical data
      if(column == 3 || column == 6){
        getmode(val_index_mode)
      }
      else{
        impute_val <- round(sum_5nn_euc / 5)
      }
      #get predicted value from the index and added to dataset
      df_euc_5nn[df_euc_5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_5nn <- sum(error_euc_5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_5nn <- (1-(error_euc_5nn/total_euc_5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_5nn)
    }
    
    #K-nn for K=5,count distance (manhattan distance)
    total_man_5nn <- 0
    error_man_5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_5nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_2[df_norm_2$Number == x1$Number ,name_column]
      #add original values
      total_man_5nn <- sum(total_man_5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sum(abs(x1 - x2))
        index <- index+1
      }
      sort_ans_man <- sort(ans,decreasing = F) [1:5]
      sum_5nn_man <- 0
      #find minimum  five distances
      for(k in 1:5){
        if(column == 3 || column == 6){
          index_5nn_man[k] <- which(sort_ans_man[k] == ans)
          val_index_mode[k] <- input[input$Number == index_5nn_man[k], name_column]
        }
        else{
          index_5nn_man[k] <- which(sort_ans_man[k] == ans)
          val_at_index <- input[input$Number == index_5nn_man[k], name_column]
          sum_5nn_man <- sum(sum_5nn_man,val_at_index)
        }
      }
      # calculate average for continuous feature and mode for categorical data
      if(column == 3 || column == 6){
        getmode(val_index_mode)
      }
      else{
        impute_val <- round(sum_5nn_man / 5)
      }
      #get predicted value from the index and added to dataset
      df_man_5nn[df_man_5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_5nn <- sum(error_man_5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_5nn <- (1-(error_man_5nn/total_man_5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_5nn)
    }
    
    
    #Weighted K-nn for K=5, count distance (euclidian distance)
    total_euc_w5nn <- 0
    error_euc_w5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_euc_w5nn) %in% name_column)]
      #get original value at index
      val_index <- df_norm_2[df_norm_2$Number == x1$Number ,name_column]
      #add original values
      total_euc_w5nn <- sum(total_euc_w5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_euc_w5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sqrt(sum((x1 - x2) ^ 2))
        index <- index+1
      }
      #find minimum five distances
      sort_ans_euc <- sort(ans,decreasing = F) [1:5]
      sum_w5nn <- 0
      sum_weight_wnn <- 0 
      for(k in 1:5){
        weight_val_euc <- 1 / sort_ans_euc[k]
        index_w5nn_euc[k] <- which(sort_ans_euc[k] == ans)
        val_at_index <- input[input$Number == index_w5nn_euc[k], name_column]
        sum_w5nn <- sum(sum_w5nn,(val_at_index*weight_val_euc))
        sum_weight_wnn <- sum(sum_weight_wnn,weight_val_euc)
      }
      #get predicted value from the index and added to dataset
      impute_val <- round(sum_w5nn / sum_weight_wnn)
      df_euc_w5nn[df_euc_w5nn$Number == x1$Number ,name_column] <- impute_val
      #find error in data by subtracting original value from dataframe and predicted value
      error_euc_w5nn <- sum(error_euc_w5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_euc_w5nn <- (1-(error_euc_w5nn/total_euc_w5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_euc_w5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_euc_w5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_euc_w5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_euc_w5nn)
    }
    
    #Weighted K-nn for K=5, count distance (manhattan distance)
    total_man_w5nn <- 0
    error_man_w5nn <- 0
    ans <- c()
    for (i in 1:nrow(y)){
      #seperate data by removing column which contains NA
      x1 <- y[i, !(names(df_man_w5nn) %in% name_column)]
      val_index <- df_norm_2[df_norm_2$Number == x1$Number ,name_column]
      #add original values
      total_man_w5nn <- sum(total_man_w5nn,val_index)
      index <- 1
      for(j in 1:nrow(input)){
        #get values of data without NA
        x2 <- input[j,!(names(df_man_w5nn) %in% name_column)]
        
        #calculate distance and add distance to a vector
        if(index %in% y$Number){
          ans[index] <- 999999
          index <- index+1
        }
        ans[index] <- sum(abs(x1 - x2))
        index <- index+1
      }
      #find minimum five distances
      sort_ans_man <- sort(ans,decreasing = F) [1:5]
      sum_w5nn <- 0
      sum_weight_wnn <- 0
      for(k in 1:5){
        weight_val_man <- 1 / sort_ans_man[k]
        index_w5nn_man[k] <- which(sort_ans_man[k] == ans)
        val_at_index <- input[input$Number == index_w5nn_man[k], name_column]
        print(val_at_index)
        sum_w5nn <- sum(sum_w5nn,(val_at_index*weight_val_man))
        sum_weight_wnn <- sum(sum_weight_wnn,weight_val_man)
      }
      #get predicted value from the index and added to dataset
      impute_val <- round(sum_w5nn / sum_weight_wnn)
      df_man_w5nn[df_man_w5nn$Number == x1$Number ,name_column] <- impute_val
      
      #find error in data by subtracting original value from dataframe and predicted value
      error_man_w5nn <- sum(error_man_w5nn,abs(impute_val-val_index))
    }
    
    #find accuracy by dividing and added value to vector which will be added to dataframe(csv file)
    accuracy_man_w5nn <- (1-(error_man_w5nn/total_man_w5nn))*100
    if(column == 3){
      Accuracy_Gender <- c(Accuracy_Gender,accuracy_man_w5nn)
    }
    if(column == 4){
      Accuracy_Age <- c(Accuracy_Age,accuracy_man_w5nn)
    }
    if(column == 5){
      Accuracy_Income <- c(Accuracy_Income,accuracy_man_w5nn)
    }
    if(column == 6){
      Accuracy_Illness <- c(Accuracy_Illness,accuracy_man_w5nn)
    }
    
  }
  
}

results_continuous <- data.frame(Method,Accuracy_Age,Accuracy_Income)
results_categorical <- data.frame(Method,Accuracy_Gender,Accuracy_Illness)

write.csv(results_continuous, file = "results_continuous.csv")
write.csv(results_categorical, file = "results_categorical.csv")


dframe1 <- read.csv("results_continuous.csv")
plot(dframe1)
dframe2 <- read.csv("results_categorical.csv")
plot(dframe2)
