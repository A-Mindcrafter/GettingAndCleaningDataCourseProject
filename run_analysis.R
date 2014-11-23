#######################################################################################################################
## Getting and Cleaning Data Course Project
## UCI HAR Dataset downloaded from
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##
## The script, run_analysis.R, does the following:
## 1. Merge train and test datasets to create one dataset.
## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## 3. Use descriptive activity names to name the activities in the data set
## 4. Appropriately label the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#######################################################################################################################

library(dplyr)

## 1. Merge train and test datasets to create one dataset
## Read the data from ./data/UCI HAR Dataset/train and also assign column names
features <- read.table("./data/UCI HAR Dataset/features.txt")
features <- features[,2]
activityType <- read.table("./data/UCI HAR Dataset/activity_labels.txt",
                           col.names = c("activityId", "activityType"))
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt",
                           col.names = "subjectId")
xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt",
                     col.names = features)
yTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt",
                     col.names = "activityId")

## Combine subjectTrain, yTrain, and xTrain
trainData <- cbind(subjectTrain, yTrain, xTrain)

## Read the data from ./test and also assign column names
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subjectId")
xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features)
yTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "activityId")

## Combine subjectTest, yTest, and xTest data
testData <- cbind(subjectTest, yTest, xTest)

## Combine train dataset and test dataset to create a final dataset
myData <- rbind(trainData, testData)
colNames <- names(myData)

## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## This will return the column indeces satisfy the search criteria
meanCol <- grep("mean..", colNames, value = FALSE, fixed = TRUE)
stdCol <- grep("std..", colNames, value = FALSE, fixed = TRUE)

## Include the first two columns, subjectId and activityId
myColumns <- c(1:2, meanCol, stdCol)
myColumns <- sort(myColumns)
subData <- myData[myColumns]

## 3. Use descriptive activity names to name the activities in the data set
subData <- merge(activityType, subData, by = "activityId")
## We have included the activity lables, so now drop (exclude) the column, activityId
subData <- subset(subData, select = - activityId)

## 4. Appropriately label the data set with descriptive variable names
## No underscores, dots or white spaces
## No duplicated
## Descriptive 
colNames <- names(subData)
colNames <- gsub("mean", "Mean", colNames, fixed = TRUE)
colNames <- gsub("std", "Std", colNames, fixed = TRUE)
colNames <- gsub("BodyBody", "Body", colNames, fixed = TRUE)
colNames <- gsub(".", "", colNames, fixed = TRUE)
colNames <- gsub("Acc", "Acceleration", colNames, fixed = TRUE)
colNames <- gsub("Mag", "Magnitude", colNames, fixed = TRUE)

## Assign the column names back to the dataset
names(subData) <- colNames

## activityType as factor variable
subData <- transform(subData, activityType = factor(activityType))

## 5. Create a second, independent tidy data set with the average of 
## each variable for each activity and each subject.

tidydata <- subData %>% group_by(subjectId, activityType) %>% summarise_each(funs(mean))

## Create a new table
write.table(tidydata, "./tidydata.txt", row.names=FALSE)