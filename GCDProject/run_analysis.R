# The purpose of this R script is to: 
## 1.Merge the training and the test sets to create one data set.
## 2.Extract only the measurements on the mean and standard deviation for each measurement. 
## 3.Use descriptive activity names to name the activities in the data set
## 4.Appropriately label the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## created in March 2015 by Laurel Gerdine()

## info on data set: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# prep work space (set directory and load packages)

if(!file.exists("./data")){dir.create("./data")}
setwd("C:/Users/Owner/Documents/Data Science/data")

library(data.table)
library(plyr)
library(dplyr)

# read in the files and merge them together to create one data set

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "C:/Users/Owner/Documents/Data Science/data/projectdatafiles.zip"
download.file(fileUrl, destfile = destFile ,method="auto")
unzip("C:/Users/OWner/Documents/Data Science/data/projectdatafiles.zip", junkpaths = TRUE, exdir = "C:/Users/OWner/Documents/Data Science/data")

features <- read.table("features.txt")

cleanupcolumnnames <- features$V2
cleanupcolumnnames <- gsub("-","",cleanupcolumnnames)
cleanupcolumnnames <- gsub(",","",cleanupcolumnnames)
cleanupcolumnnames <- gsub("(","",cleanupcolumnnames, fixed = TRUE)
cleanupcolumnnames <- gsub(")","",cleanupcolumnnames)
features$V2 <- cleanupcolumnnames
featurenames <- features[,2]

activity_labels <- read.table("activity_labels.txt")
subject_test <- read.table("subject_test.txt")
X_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_train <- read.table("subject_train.txt")
X_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")

# replace column names with feature names (more descriptive)

colnames(X_test) <- featurenames
colnames(X_train) <- featurenames
colnames(y_train) <- c("Activity")
colnames(y_test) <- c("Activity")
colnames(subject_test) <- c("Subject")
colnames(subject_train) <- c("Subject")
colnames(activity_labels) <- c("Activity","ActivityType")

# merge files

trainmerge <- cbind (subject_train, y_train, X_train)
testmerge <- cbind(subject_test, y_test, X_test)
wholemerge <- rbind(trainmerge,testmerge)
dt <- wholemerge

# extract columns that contain mean and std - ALL columns that contain some form of mean or std

meancolumns <- dt [ , grepl("mean", names(dt), fixed = TRUE)] ## selected all columns that contained some type of mean
stdcolumns <- dt [ , grepl("std", names(dt), fixed = TRUE)] ## selected all columns that contained some type of std
idcolumns <- dt[,1:2]
dt1 <- cbind(idcolumns,meancolumns,stdcolumns)

# label the data set with the descriptive name variables for activities

dt2 <-join(dt1, activity_labels, by = "Activity" )
activity_type <- dt2$ActivityType
dt2$Activity <- activity_type
dt2 <- dt2[,-82]

# create a second independent data set with the average of each variable 
# for each activity and each subject

dt3 <- dt2 %>% group_by(Subject, Activity) %>%
  summarise_each(funs(mean, "mean", mean(., na.rm = TRUE)))

# done!
