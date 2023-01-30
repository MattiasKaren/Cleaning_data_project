
## Get the data:
library(data.table)
library(dplyr)



## Get the supporting metadata into data frames:
featurenames <- read.table("UCI HAR Dataset/features.txt")
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

##Get the training data: 
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## Get the testing data:
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

##Merging the data into one data frame
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

## Naming the columns using the metadata:
colnames(features) <- t(featurenames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

#Merge the data into one complete Dataset. 
completeData <- cbind(features,activity,subject)


## Extract only the columnds for mean and std: 

TidyDataset <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

## Adding columns for 'subject' and 'activity:
tidyColumns <- c(TidyDataset, 562, 563)

## Getting the extracted data we need: 
extractedData <- completeData[,tidyColumns]

##Changing 'activity' column from numeric to character. 
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

##Factoring the 'activity' variable:
extractedData$Activity <- as.factor(extractedData$Activity)

## Changing the variable names to more descriptive names:

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

## Part 5: Creating a second data set with the average of each variable: 
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

FinalData <- extractedData %>%
  group_by(Subject, Activity) %>%
  summarise_all(mean)
FinalData <- FinalData[order(FinalData$Subject,FinalData$Activity),]

## Write final data as a txt file: 
write.table(FinalData, "FinalData.txt", row.name=FALSE)

install.packages("dataMaid")
library(dataMaid)
makeCodebook(FinalData, replace = TRUE)
