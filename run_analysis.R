#Data download and unzip

#Creates directory "project" if it does not exist
if (!file.exists("project")) {
        dir.create("project")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "./project/UCIdata.zip"
dir <- "UCI HAR Dataset"

#Checks if the file to download exists and if not downloads the file
if(!file.exists(filename)){
        download.file(fileUrl, destfile = filename, mode = "wb")    
}

#Checks if the unzip directory exits and if not unzips the .zip file into the "project" dir
if(!file.exists(dir)){
        unzip(filename, files = NULL, exdir="./project")
}

## Read Data
subject_test <- read.table("./project/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./project/UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("./project/UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("./project/UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("./project/UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("./project/UCI HAR Dataset/train/y_train.txt")

activity_labels <- read.table("./project/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./project/UCI HAR Dataset/features.txt")  

#Assign column names to the data above.
colnames(activity_labels)<-c("activityId","activityType")
colnames(subject_train) <- "subId"
colnames(X_train) <- features[,2]
colnames(y_train) <- "activityId"
colnames(subject_test) <- "subId"
colnames(X_test) <- features[,2]
colnames(y_test) <- "activityId"

#Analysis
#1. Merging training Data
trainData <- cbind(y_train,subject_train,X_train)

#Merging test Data
testData <- cbind(y_test,subject_test,X_test)

#Final merged Data
finalData <- rbind(trainData,testData)

#2. Extracts only the measurements on the mean and standard deviation for each measurement
# creating a vector for column names to be used further
colNames <- colnames(finalData)

data_mean_std <-finalData[,grepl("mean|std|subId|activityId",colnames(finalData))]

#3. Uses descriptive activity names to name the activities in the data set
library(plyr)
data_mean_std <- join(data_mean_std, activity_labels, by = "activityId", match = "first")
data_mean_std <-data_mean_std[,-1]

#4. Appropriately labels the data set with descriptive variable names.
#Remove parentheses
names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)

#correct syntax in names
names(data_mean_std) <- make.names(names(data_mean_std))

#add descriptive names
names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidydata_average_sub<- ddply(data_mean_std, c("subId","activityType"), numcolwise(mean))
write.table(tidydata_average_sub,file="tidydata.txt")

