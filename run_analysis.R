## Peng Yan
## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

## in case you are not sure, 
## you can double check with the github repo 
## where this file is based in
##  You should create one R script called run_analysis.R that does the following. 

## 1) Merges the training and the test sets to create one data set

  merge_data <- function(datafolder) {
 
  ## use one variable path as the current path for different uses

 ## load the test data  
  path <- paste( datafolder, "/test/X_test.txt", sep="")
  test_data <- read.table(path)
## load the training data  

  path <- paste( datafolder, "/train/X_train.txt", sep="")
  train_data <- read.table(path)
  
  
  ## read the test and training subject names
  path <- paste(datafolder, "/train/subject_train.txt", sep="")
  subject_train <- read.table(path)
  path <- paste( datafolder, "/test/subject_test.txt", sep="")
  subject_test <- read.table(path)
  
  ## read the activity names from the file
  path <- paste( datafolder, "/activity_labels.txt", sep="")
  activity_labels <- read.table(path)

  ## read the test and training y labels
  path <- paste(datafolder, "/train/y_train.txt", sep="")
  y_train <- read.table(path)
  path <- paste(datafolder, "/test/y_test.txt", sep="")
  y_test <- read.table(path)
  
  ## merge y test and training activity labels
  y_train_labels <- merge(y_train,activity_labels,by="V1")
  y_test_labels <- merge(y_test,activity_labels,by="V1")
  
  ## merge the test and training data and the respective labels together
  train_data <- cbind(subject_train,y_train_labels,train_data)
  test_data <- cbind(subject_test,y_test_labels,test_data)
  
  ## merge the test and training data

  merged_data <- rbind(train_data,test_data)
  ### return the result
  return (merged_data)
}

## 2. Extracts only the measurements on the mean and standard deviation for each measurement

  extract_mean_std <- function(data_set, datafolder) {
  path <- paste( datafolder, "/features.txt", sep="")
  features_data <- read.table(path)


  ## subset only those rows where the name contains the word mean and std
  
   mean_std_rows <- subset(features_data,  grepl("(mean\\(\\)|std\\(\\))", features_data$V2) )
  
  ## set the column headers for combined data with Subject, activity_id, activity
  colnames(data_set) <- c("Subject","Activity_Id","Activity",as.vector(features_data[,2]))
  
  ## extract the data from the merged data where the column names are mean OR std
  mean_columns <- grep("mean()", colnames(data_set), fixed=TRUE)
  std_columns <- grep("std()", colnames(data_set), fixed=TRUE)
  
  ## put both mean and std columns into a vector
  mean_std_column_vector <- c(mean_columns, std_columns)
  
  ## sort the vector 
  mean_std_column_vector <- sort(mean_std_column_vector)
  
  ## extract the columns with std and mean in their column headers
  extracted_data<- data_set[,c(1,2,3,mean_std_column_vector)]
  return (extracted_data)
}

## 3. Uses descriptive activity names to name the activities in the data set
##    the merge_data and extract_mean_std functions are used
## 4. Appropriately labels the data set with descriptive variable names. 
## 	the merge_data and extract_mean_std functions are used
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

  melt_data_and_write_tidy_set <- function(data_set, path_to_tidyset_file) {
  ## let's melt the data
  require(reshape2)
  melted_data <- melt(data_set, id=c("Subject","Activity_Id","Activity"))
  
  ## cast the data back to the tidy_data format
  tidy_data <- dcast(melted_data, formula = Subject + Activity_Id + Activity ~ variable, mean)
  
  ## format the column names
  col_names_vector <- colnames(tidy_data)
  col_names_vector <- gsub("-mean()","Mean",col_names_vector,fixed=TRUE)
  col_names_vector <- gsub("-std()","Std",col_names_vector,fixed=TRUE)
  col_names_vector <- gsub("BodyBody","Body",col_names_vector,fixed=TRUE)
  
  ## put back in the tidy column names
  colnames(tidy_data) <- col_names_vector
  
  ## export the output into a file to the working datafolder
  write.table(tidy_data, file=path_to_tidyset_file, sep="\t", row.names=FALSE)
}


## calling the functions to perform the required tasks by passing the data datafolder information

merged_data <- merge_data("getdata-projectfiles/UCIHARDataset")
extracted_mean_std_data_set <- extract_mean_std(merged_data, "getdata-projectfiles/UCIHARDataset")
melt_data_and_write_tidy_set(extracted_mean_std_data_set, "./tidyset.txt")

