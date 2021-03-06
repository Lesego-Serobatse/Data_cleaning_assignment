---
title: "Codebook"
author: "Lesego Serobatse"
date: "24/10/2021"
output: html_document
---

## Description of data

This data contains averages of means and standard deviations of 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz calculated from raw data captured using an embedded accelerometer and gyroscope on a smartphone (Samsung Galaxy S II) worn by 30 volunteers with varying ages in the range of 19-48. The data was captured whilst volunteers performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING), where 70% of the volunteers was selected for generating the training data and 30% the test data.

Each record contains:
 - The volunteer identity
 - The type of activity
 - Total linear body acceleration
 - Tri-axial linear body acceleration
 - Total gravity acceleration
 - Tri-axial gravity acceleration
 - Tri-axial rate of linear body acceleration change
 - Total angular body velocity
 - Tri-axial angular body velocity
 - Tri-axial rate of angular body velocity change
 - Tri-axial frequency of linear body acceleration 
 - Tri-axial frequency of rate of linear body acceleration change
 - Tri-axial frequency of angular body velocity

In total the data contains 81 columns and 180 records 



## Description of variables

The variables in the data are essentially the averages of means, standard deviations and mean-frequency (from raw data) for body acceleration, gravity acceleration and angular velocity, furthermore it dissects those variables linearly in a tri-axial pattern along the x-y-z axis. These averages were calculated from the raw data. There are also activity and volunteer unique identities performed by thirty participants. There exist 81 variables in total, a snippet of which include: 
 - The volunteer identity
 - The type of activity
 - Total linear body acceleration
 - Tri-axial linear body acceleration
 - Total gravity acceleration
 - Tri-axial gravity acceleration
 - Tri-axial rate of linear body acceleration change
 - Total angular body velocity
 - Tri-axial angular body velocity
 - Tri-axial rate of angular body velocity change
 - Tri-axial frequency of linear body acceleration 
 - Tri-axial frequency of rate of linear body acceleration change
 - Tri-axial frequency of angular body velocity



## Data transformation description and code

The raw data 

### Merging the training and the test sets to create one data set.


Step 1: Read the activity label data from the text file and store in a dataframe
```{r}
activity_labels <- read.table(file = "./assignment/UCI HAR Dataset/activity_labels.txt")
```


Step 2: Read the features data from the text file and store in a dataframe
```{r}
featuress <- read.table(file = "./assignment/UCI HAR Dataset/features.txt")
```


Step 3: Read the test data from the text file and store in dataframe
```{r}
X_test_data <- read.table("./assignment/UCI HAR Dataset/test/X_test.txt")
```


Step 4: Read the activity codes data from the text file and store in dataframe
```{r}
Y_test_data <- read.table("./assignment/UCI HAR Dataset/test/Y_test.txt")
```


Step 5: Read the volunteer-id codes from the text file and store in dataframe
```{r}
subject_test_data <- read.table("./assignment/UCI HAR Dataset/test/subject_test.txt")
```


Step 6: Assign column names for the test data from the second column of the features dataframe
```{r}
names(X_test_data) <- featuress$V2
```


Step 7: Assign column name for the subject test data
```{r}
names(subject_test_data) <- c("volunteer_id")
```


Step 8: Joining the activity labels to the activity codes based on activity codes
```{r}
test_activity_vector_codes_and_labels <- left_join(Y_test_data, activity_labels, by = c("V1" = "V1"))
```


Step 9: Renaming the activity_vector_codes_and_labels dataframe appropriately
```{r}
names(test_activity_vector_codes_and_labels) <- c("code", "activity")
```


Step 10: Merging the test activity vector from the second column of 'activity_vector_codes_and_labels'. dataframe to the test data and store results in a new dataframe 'partially_complete_test_data_1'
```{r}
partially_complete_test_data_1 <- cbind(test_activity_vector_codes_and_labels$activity, X_test_data)
```


Step 11: Renaming the newly inserted column in 'partially_complete_test_data_1' according to its original column name from its previous dataframe 'activity_vector_codes_and_labels'
```{r}
names(partially_complete_test_data_1)[1] <- names(test_activity_vector_codes_and_labels)[2]
```


Step 12: Create a dataframe describing the type of set the row contains and this will be 'test' for all the columns
```{r}
test_set_type <- character(0)
for(i in 1:nrow(partially_complete_test_data_1)){
  test_set_type[i] <- "test"
}
test_set_type <- as.data.frame(test_set_type)
names(test_set_type) <- c("set_type")
```


Step 13: Merge the set_type dataframe with the 'partially_complete_test_data_1' data and store the results in a new dataframe called 'partially_complete_test_data_2'
```{r}
partially_complete_test_data_2 <- cbind.data.frame(set_type, partially_complete_test_data_1)
```


Step 14: Merge the subject_test_data with the partially_complete_test_data_2 data and store the results in a new dataframe called complete_test_data 
```{r}
complete_test_data <- cbind(subject_test_data, partially_complete_test_data_2)
```


Repeat the same exact i.e steps 3-14, for the train data
```{r}
X_train_data <- read.table("./assignment/UCI HAR Dataset/train/X_train.txt")
Y_train_data <- read.table("./assignment/UCI HAR Dataset/train/Y_train.txt")
subject_train_data <- read.table("./assignment/UCI HAR Dataset/train/subject_train.txt")
names(X_train_data) <- featuress$V2
names(subject_train_data) <- c("volunteer_id")
train_activity_vector_codes_and_labels <- left_join(Y_train_data, activity_labels, by = c("V1" = "V1"))
names(train_activity_vector_codes_and_labels) <- c("code", "activity")
partially_complete_train_data_1 <- cbind(train_activity_vector_codes_and_labels$activity, X_train_data)
names(partially_complete_train_data_1)[1] <- names(train_activity_vector_codes_and_labels)[2]
train_set_type <- character(0)
for(i in 1:nrow(partially_complete_train_data_1)){
  train_set_type[i] <- "train"
}
train_set_type <- as.data.frame(train_set_type)
names(train_set_type) <- c("set_type")
partially_complete_train_data_2 <- cbind.data.frame(train_set_type, partially_complete_train_data_1)
complete_train_data <- cbind(subject_train_data, partially_complete_train_data_2)
```


Building the complete dataset. 
Merging the train data with the test data to form a complete dataset.
```{r}
complete_data <- rbind.data.frame(complete_train_data, complete_test_data)
```



### Extracts only the measurements on the mean and standard deviation for each measurement.


Retrieving the column positions for the measurements on the mean
```{r}
mean_column_positions <- as.data.frame(grep("mean", names(complete_data)))
names(mean_column_positions) <- "column_positions"
```


Retrieving the column positions for the measurements on the standard deviation
```{r}
std_column_positions <- as.data.frame(grep("std", names(complete_data)))
names(std_column_positions) <- "column_positions"
```


Merging the column positions for the measurements on the mean and standard deviation
Sort by ascending order
```{r}
mean_std_column_positions <- rbind(mean_column_positions, std_column_positions)
mean_std_column_positions <- mean_std_column_positions %>% arrange(column_positions)
```


Extracting only the measurements on the mean and standard deviation for each measurement
Dataframe for the information above, including the first three columns
```{r}
mean_std_df <- complete_data[,c(1, 2, 3, mean_std_column_positions$column_positions)]
```



## creates a second, independent tidy data set with the average of each variable for each activity and each subject.


Function definition that groups and calculates the mean per activity and subject
```{r}
mean_per_activity_and_subject <- function(dat, subject = mean_std_df$volunteer_id, 
                                          activity = mean_std_df$activity){
                                  mean_results <- tapply(dat, list(subject, activity), mean)
                                  mean_results_df <- as.data.frame(mean_results)
                                  mean_results_df_1 <- cbind.data.frame(as.data.frame(c(1:30)), mean_results_df)
                                  names(mean_results_df_1)[1] <- "volunteer_id"
                                  mean_results_final <- melt(mean_results_df_1, id.vars = c("volunteer_id"), 
                                                        measure.vars = names(mean_results_df_1)[2:7], 
                                                        variable.name = "activity", value.name = "mean")
                                  return(mean_results_final)
}

```


Extracting the list of variables and variable names
```{r}
variables_list <- as.list(mean_std_df[,4:82])
```


Calculating the means/averages of different variables
```{r}
average_of_variables <- lapply(variables_list, FUN = mean_per_activity_and_subject)
```


Converting the average of variables list into a dataframe
```{r}
for(i in 1:length(average_of_variables)){
    temp <- as.data.frame(average_of_variables[[i]])
    names(temp)[3] <- paste("average", "of", names(average_of_variables)[i])
    if(i == 1){
      final_clean_average_of_variables_df <- temp
    } else {
        final_clean_average_of_variables_df <- left_join(final_clean_average_of_variables_df,
                                                         temp, by = c("volunteer_id", "activity"))
    }
}

```



### Renaming the columns appropriately

Naming the columns appropriately
```{r}
for(i in 3:81){
  initial_column_name <- gsub("-", " ",names(final_clean_average_of_variables_df)[i])
  
  column_name_words <- strsplit(initial_column_name," ")
  
  if(grepl("mean", column_name_words[[1]][4]) == TRUE){
      final_column_name <- paste("average of", "means for")
  } else if(grepl("std", column_name_words[[1]][4]) == TRUE) {
      final_column_name <- paste("average of", "standard deviations for")    
  }
  
  if(grepl("tBodyAcc", column_name_words[[1]][3]) == TRUE){
      final_column_name <- paste(final_column_name, "linear body acceleration")
  } else if(grepl("tGravityAcc", column_name_words[[1]][3]) == TRUE){
      final_column_name <- paste(final_column_name, "gravity acceleration")
  } else if(grepl("tBodyAccJerk", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "rate of linear body acceleration change")
  } else if(grepl("tBodyGyro", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "angular body velocity")
  } else if(grepl("tBodyGyroJerk", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "rate of angular body velocity change")
  } else if(grepl("tBodyAccMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "Magnitude of linear body acceleration")
  } else if(grepl("tGravityAccMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "Magnitude of gravity acceleration")
  } else if(grepl("tBodyAccJerkMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "Magnitude of rate of linear body acceleration change")
  } else if(grepl("tBodyGyroMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "Magnitude of angular body velocity")
  } else if(grepl("tBodyGyroJerkMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "Magnitude of rate of angular body velocity change")
  } else if(grepl("fBodyAcc", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency of linear body acceleration")
  } else if(grepl("fBodyAccJerk", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency of rate of linear body acceleration change")
  } else if(grepl("fBodyGyro", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency of angular body velocity")
  } else if(grepl("fBodyAccMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency magnitude of linear body acceleration")
  } else if(grepl("fBodyAccJerkMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency magnitude of rate of linear body acceleration change")
  } else if(grepl("fBodyGyroMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency magnitude of angular body velocity")
  } else if(grepl("fBodyGyroJerkMag", column_name_words[[1]][3]) == TRUE){
    final_column_name <- paste(final_column_name, "frequency magnitude of rate of angular body velocity change")
  }
  
  if(grepl("X", column_name_words[[1]][5]) == TRUE){
      final_column_name <- paste(final_column_name, "on the x-axis")
  } else if(grepl("Y", column_name_words[[1]][5]) == TRUE){
    final_column_name <- paste(final_column_name, "on the y-axis")
  } else if(grepl("Z", column_name_words[[1]][5]) == TRUE){
    final_column_name <- paste(final_column_name, "on the z-axis")
  } 

  names(final_clean_average_of_variables_df)[i] <- final_column_name
  
}


```



Final cleaned dataset
```{r}
Final_clean_data <- final_clean_average_of_variables_df

```


View the final cleaned dataset
```{r}
View(Final_clean_data)
```

Wrapping the Final cleaned dataset in a csv file
```{r}
write.table(Final_clean_data, "./assignment/tidy_dataset.csv", sep = ",",
            col.names = TRUE, qmethod = "double")

```



