# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#    measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.


# 1. Merges the training and the test sets to create one data set.

# set working directory
setwd("~/Coursera/03_Getting and Cleaning Data/Week 04/Week 4 Assignment")

# cheking for and creating directories
if (!file.exists("./01_Data")) {
        dir.create("./01_Data")
}

# download dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = fileUrl, destfile = "./01_Data/Dataset.zip")
dateDownloaded <- date()
unzip(zipfile = "./01_Data/Dataset.zip", exdir = "./01_Data")

# read dataset for Training
TrainingSet <- read.csv(file = "./01_Data/UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
TrainingLabels <- read.csv(file = "./01_Data/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
TrainingSubject <- read.csv(file = "./01_Data/UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")

# read dataset for Test
TestSet <- read.csv(file = "./01_Data/UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
TestLabels <- read.csv(file = "./01_Data/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
TestSubject <- read.csv(file = "./01_Data/UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")

# read complementary dataset
Features <- read.csv(file = "./01_Data/UCI HAR Dataset/features.txt", header = FALSE, sep = "")
ActivityLabels <- read.csv(file = "./01_Data/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")

# add column name
colnames(x = TrainingSet) <- Features$V2
colnames(x = TestSet) <- Features$V2
colnames(x = TrainingSubject) <- "subjectID"
colnames(x = TestSubject) <- "subjectID"
colnames(x = TrainingLabels) <- "activityID"
colnames(x = TestLabels) <- "activityID"

# merge data
TrainingData <- cbind(TrainingSubject, TrainingLabels, TrainingSet)
TestData <- cbind(TestSubject, TestLabels, TestSet)
tidy1 <- rbind(TrainingData, TestData)


# 2. Extracts only the measurements on the mean and standard deviation for each 
#    measurement.

# detect if column name contain "mean" or "sd"
keep <- grepl("mean\\(\\)", names(tidy1)) | grepl("std\\(\\)", names(tidy1))

# keep the subject and activity column
keep[1:2] <- TRUE

# data with measurements on the mean and sd
tidy1 <- tidy1[, keep]


# 3. Uses descriptive activity names to name the activities in the data set

# name the activity variable
for (i in 1:6){
        tidy1$activity[tidy1$activityID == i] <- as.character(ActivityLabels[i,2])
}

tidy1$activity <- as.factor(tidy1$activity)

# move the activity column after the activity id column
tidy1 <- tidy1[, c(1, 2, 69, 3:68)]


# 4. Appropriately labels the data set with descriptive variable names.

# rename column name
names(tidy1) <- gsub(pattern = "^t", replacement = "Time", x = names(tidy1))
names(tidy1) <- gsub(pattern = "^f", replacement = "Frequency", x = names(tidy1))
names(tidy1) <- gsub(pattern = "-mean\\(\\)", replacement = "_Mean", x = names(tidy1))
names(tidy1) <- gsub(pattern = "-std\\(\\)", replacement = "_StdDev", x = names(tidy1))
names(tidy1) <- gsub(pattern = "Mag", replacement = "Magnitude", x = names(tidy1))
names(tidy1) <- gsub(pattern = "Acc", replacement = "Accelerometer", x = names(tidy1))
names(tidy1) <- gsub(pattern = "Gyro", replacement = "Gyroscope", x = names(tidy1))


# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# summary means
tidy2 <- aggregate(formula = . ~subjectID + activity, data = tidy1, FUN = mean)

# order by subject id
tidy2 <- tidy2[order(tidy2[,"subjectID"], na.last = TRUE, decreasing = FALSE),]

# swap activity id and activity column
tidy2 <- tidy2[, c(1, 3, 2, 4:69)]

# cheking for and creating directories
if (!file.exists("./02_Output")) {
        dir.create("./02_Output")
}

# export to .txt file
write.table(tidy2, file = "./02_Output/tidy.txt",row.name=FALSE)
