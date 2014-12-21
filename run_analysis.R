library(plyr)
  # Set Working Directory

  setwd("C:/Kanchan/Getting and Cleaning Data/week3/")

  mergeDatasets = function() {
  
  # Read and merge data

  message("reading X_train.txt")
  training.x <- read.table("UCI HAR Dataset/train/X_train.txt")
  
  message("reading y_train.txt")
  training.y <- read.table("UCI HAR Dataset/train/y_train.txt")
  
  message("reading subject_train.txt")
  training.subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  message("reading X_test.txt")
  test.x <- read.table("UCI HAR Dataset/test/X_test.txt")
  
  message("reading y_test.txt")
  test.y <- read.table("UCI HAR Dataset/test/y_test.txt")
  
  message("reading subject_test.txt")
  test.subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  # Merge
  merged.x <- rbind(training.x, test.x)
  merged.y <- rbind(training.y, test.y)
  merged.subject <- rbind(training.subject, test.subject)
  message("Merging Complete")
  
  # merge train and test datasets and return
  list(x=merged.x, y=merged.y, subject=merged.subject)
}

extractMeanandStd = function(df) {
  # Given the dataset (x values), extract only the measurements on the mean
  # and standard deviation for each measurement.
  
  # Read the feature list file
  features <- read.table("UCI HAR Dataset/features.txt")
  
  # Find the mean and std columns
  mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
  std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
  
  # Extract them from the data
  edf <- df[, (mean.col | std.col)]
  colnames(edf) <- features[(mean.col | std.col), 2]
  edf
}

nameActivities = function(df) {
  # Use descriptive activity names to name the activities in the dataset
  colnames(df) <- "activity"
  df$activity[df$activity == 1] = "WALKING"
  df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
  df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
  df$activity[df$activity == 4] = "SITTING"
  df$activity[df$activity == 5] = "STANDING"
  df$activity[df$activity == 6] = "LAYING"
  df
}

bindData <- function(x, y, subjects) {
  # Combine mean-std values (x), activities (y) and subjects into one data
  # frame.
  cbind(x, y, subjects)
}

createTidyDataset = function(df) {
  # Given X values, y values and subjects, create an independent tidy dataset
  # with the average of each variable for each activity and each subject.
  tidy <- ddply(df, .(subject, activity), function(x) colMeans(x[,1:60]))
  tidy
}

clean.data = function() {
  
  #1. Merges the training and the test sets to create one data set
  merged <- mergeDatasets()
  
  #2.Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  cx <- extractMeanandStd(merged$x)
    
  # 3.Uses descriptive activity names to name the activities in the data set
  cy <- nameActivities(merged$y)
  
  # 4.Appropriately labels the data set with descriptive variable names. 
  colnames(merged$subject) <- c("subject")
  
  #Combine data frames into one
  combined <- bindData(cx, cy, merged$subject)
  
  #Create tidy dataset
  tidy <- createTidyDataset(combined)
  
  #Write tidy dataset as csv
  write.csv(tidy, "UCI_HAR_tidy.csv", row.names=FALSE)
  write.table(tidy, "tidy.txt", sep="\t")
  
}
