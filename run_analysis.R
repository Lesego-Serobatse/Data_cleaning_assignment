##******************************************************************************
##        Merges the training and the test sets to create one data set.
##******************************************************************************

## STEP: 1
## Read the activity label data from the text file and store in dataframe
activity_labels <- read.table(file = "./assignment/UCI HAR Dataset/activity_labels.txt")

## STEP: 2
## Read the features data from the text file and store in dataframe 
featuress <- read.table(file = "./assignment/UCI HAR Dataset/features.txt")

## STEP: 3
## Read the test data from the text file and store in dataframe
X_test_data <- read.table("./assignment/UCI HAR Dataset/test/X_test.txt")

## STEP: 4
## Read the activity codes data from the text file and store in dataframe 
Y_test_data <- read.table("./assignment/UCI HAR Dataset/test/Y_test.txt")

## STEP: 5
## Read the volunteer-id codes from the text file and store in dataframe
subject_test_data <- read.table("./assignment/UCI HAR Dataset/test/subject_test.txt")

## STEP: 6
## Assign column names for the test data from the second column of the features dataframe
names(X_test_data) <- featuress$V2  

## STEP: 7
## Assign column name for the subject test data
names(subject_test_data) <- c("volunteer_id") 

## STEP: 8
## Joining the activity labels to the activity codes based on activity codes
test_activity_vector_codes_and_labels <- left_join(Y_test_data, activity_labels, by = c("V1" = "V1"))

## STEP: 9
## Renaming the activity_vector_codes_and_labels dataframe appropriately
names(test_activity_vector_codes_and_labels) <- c("code", "activity")

## STEP: 10
## Merging the test activity vector from the second column of 'activity_vector_codes_and_labels'
## dataframe to the test data and store results in a new dataframe 'partially_complete_test_data_1'
partially_complete_test_data_1 <- cbind(test_activity_vector_codes_and_labels$activity, X_test_data)

## STEP: 11
## Renaming the newly inserted column in 'partially_complete_test_data_1' according to its original column name
## from its previous dataframe 'activity_vector_codes_and_labels'
names(partially_complete_test_data_1)[1] <- names(test_activity_vector_codes_and_labels)[2]

## STEP: 12
## Create a dataframe describing the type of set the row contains and this will be 'test'
## for all the columns
test_set_type <- character(0)
for(i in 1:nrow(partially_complete_test_data_1)){
  test_set_type[i] <- "test"
}
test_set_type <- as.data.frame(test_set_type)
names(test_set_type) <- c("set_type")

## STEP: 13
## Merge the set_type dataframe with the 'partially_complete_test_data_1' data and store
## the results in a new dataframe called ''
partially_complete_test_data_2 <- cbind.data.frame(test_set_type, partially_complete_test_data_1)

## STEP: 14
## Merge the subject_test_data with the partially_complete_test_data_2 data and store 
## the results in a new dataframe called complete_test_data
complete_test_data <- cbind(subject_test_data, partially_complete_test_data_2)


## Repeat steps 3-14 upon the train data
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


## Building the complete dataset
## Merging the train data with the test data to form a complete dataset
complete_data <- rbind.data.frame(complete_train_data, complete_test_data)

View(complete_data)


##*****************************************************************************************
##Extracts only the measurements on the mean and standard deviation for each measurement. 
##*****************************************************************************************

## Retrieving the column positions for the measurements on the mean
mean_column_positions <- as.data.frame(grep("mean", names(complete_data)))
names(mean_column_positions) <- "column_positions"

## Retrieving the column positions for the measurements on the standard deviation
std_column_positions <- as.data.frame(grep("std", names(complete_data)))
names(std_column_positions) <- "column_positions"

## Merging the column positions for the measurements on the mean and standard deviation
## Sort by ascending order
mean_std_column_positions <- rbind(mean_column_positions, std_column_positions)
mean_std_column_positions <- mean_std_column_positions %>% arrange(column_positions)

## Extracting only the measurements on the mean and standard deviation for each measurement
## Dataframe for the information above, including the first three columns
mean_std_df <- complete_data[,c(1, 2, 3, mean_std_column_positions$column_positions)]

View(mean_std_df)


##*****************************************************************************************
## creates a second, independent tidy data set with the average of each variable for each 
## activity and each subject.
##*****************************************************************************************

## Function definition that groups and calculates the mean per activity and subject
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

##Extracting the list of variables and variable names
variables_list <- as.list(mean_std_df[,4:82])

##Calculating the means/averages of different variables
average_of_variables <- lapply(variables_list, FUN = mean_per_activity_and_subject)


##Converting the average of variables list into a dataframe

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

View(final_clean_average_of_variables_df)



##******************************************************************************
##     Renaming the columns appropriately
##******************************************************************************

## Naming the columns appropriately
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


View(final_clean_average_of_variables_df)

## Final cleaned dataset
Final_clean_data <- final_clean_average_of_variables_df
View(Final_clean_data)

## Wrapping the Final clean data dataframe in a csv file
write.csv(Final_clean_data, "./assignment/tidy_dataset.csv")



