##1.Merges the training and the test sets to create one data set.

runAnalysis <- function() {
  library(data.table)
  library(dplyr)
  
  ## Read files
  xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt",sep="",dec=".")
  yTrain <- read.table("./data/UCI HAR Dataset/train/Y_train.txt",sep="",dec=".")
  subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt",sep="",dec=".")
  xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt",sep="",dec=".")
  yTest <- read.table("./data/UCI HAR Dataset/test/Y_test.txt",sep="",dec=".")
  subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt",sep="",dec=".")
  
  features <- read.table("./data/UCI HAR Dataset/features.txt",sep="",dec=".")
  
  # combine files by column
  train <- cbind(subjectTrain,yTrain,xTrain)
  test <- cbind(subjectTest,yTest,xTest)
  
  # combine files per row
  combined <- rbind(test,train)
  # add column names
  colnames(combined) <- c("Subject","Activity",c(make.unique(as.vector(features[,2]))))

  
##2.Extracts only the measurements on the mean and standard deviation for each measurement. 

  
meanStd <- select(combined,Subject,Activity,contains("mean()"),contains("std()"))

##3.Uses descriptive activity names to name the activities in the data set

meanStd$Activity[meanStd$Activity==1] <- "WALKING"
meanStd$Activity[meanStd$Activity==2] <- "WALKING_UPSTAIRS"
meanStd$Activity[meanStd$Activity==3] <- "WALKING_DOWNSTAIRS"
meanStd$Activity[meanStd$Activity==4] <- "SITTING"
meanStd$Activity[meanStd$Activity==5] <- "STANDING"
meanStd$Activity[meanStd$Activity==6] <- "LAYING"

##4.Appropriately labels the data set with descriptive variable names. 

## Already done i in 1.

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

grouped <- group_by(meanStd,Activity,Subject)
summarise_each(grouped,funs(mean))
# Uncomment to create file
##write.table(summarise_each(grouped,funs(mean)),row.name=FALSE,file="./data/assignment_result.txt")

}


