# ProgrammingAssignment3
A Peer Graded Assignment based on Coursera Data Cleaning course by Alfred T.

# Instruction
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.

Review criterialess 
The submitted data set is tidy.
The Github repo contains the required scripts.
GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
The README that explains the analysis files is clear and understandable.
The work submitted for this project is the work of the student who submitted it.

# Getting and Cleaning Data Course Projectless 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Code Breakdown

## Part1: Get_data: Get required data into a list
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, destfile = dest_file) #
  unzip(dest_file, exdir = ex_dir) #
  file_names <- c("train/X_train.txt", "train/y_train.txt", "train/subject_train.txt", "test/X_test.txt", "test/y_test.txt", "test/subject_test.txt", "features.txt")
  file_paths <- paste0("~/UCI HAR Dataset/", file_names)
  df_names <- c("X_train", "y_train", "subject_train", "X_test", "y_test", "subject_test", "features")
  df_list <- list() 
  for (i in 1:length(file_paths)) {
    df_list[[i]] <- read.table(file_paths[i], stringsAsFactors = FALSE) # Read into text file
  }
  names(df_list) <- df_names

## Part2: Merge_data: Merges the training and the test sets to create one data set
  X <- rbind(df_list$X_train, df_list$X_test) #Combine Row of x_train and x_test
  y <- rbind(df_list$y_train, df_list$y_test) #Combine Row of y_train and y_test
  subject <- rbind(df_list$subject_train, df_list$subject_test) # Combine Row of subject_train and subject_test
  df <- cbind(y, X) # Combine column of x and y
  df <- cbind(subject, df) # Combine column of x,y and subject


## Part3: Extract_data: Extracts only the measurements on the mean and standard deviation for each measurement
  feature_names <- df_list$features$V2
  has_mean_std <- feature_names %>%
    grepl(pattern = "mean|std") # Set criteria for features with mean or std
  has_meanFreq <- feature_names %>%
    grepl(pattern = "meanFreq()") # Set criteria for features with meanFreq
  feature_selector <- has_mean_std & !has_meanFreq
  feature_selector <- c(TRUE, TRUE, feature_selector) # Filter subject ID with mean or std and without meanFreq
  mean_std_df <- df[, feature_selector]
  return(mean_std_df)

## Part4: Label_data: Uses descriptive activity names to name the activities in the data set
  activities <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  for (i in 1:length(activities)) {
    mean_std_df[, 2][mean_std_df[, 2] == i] <- activities[i]
  }

## Part5: :Label_variable_names: Appropriately labels the data set with descriptive variable names
  feature_names <- df_list$features$V2
  has_mean_std <- feature_names %>%
    grepl(pattern = "mean()|std()")
  has_meanFreq <- feature_names %>%
    grepl(pattern = "meanFreq()")
  feature_selector <- has_mean_std & !has_meanFreq
  variable_names <- c("subject", "activity", feature_names[feature_selector])
  names(mean_std_df) <- variable_names


## Part6: Summarize_data: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
  tidy_data <- mean_std_df %>% 
    group_by(subject, activity) %>%
    summarise_all(mean)
  save(tidy_data, file='tidySet.Rdata')
  write.table(tidy_data, file='tidySet.txt')
