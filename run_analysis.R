library(dplyr)

#Read Train Data
X_Train <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/X_train.txt")
Y_Train <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/y_train.txt")
Subject_Train <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/subject_train.txt")

#Read Test Data
X_Test <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/X_test.txt")
Y_Test <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/y_test.txt")
Subject_Test <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/subject_test.txt")

#Read Data Descriptoin
Variable_Names <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/features.txt")

#Read Activity Labels
Activity_Labels <- read.table("/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/activity_labels.txt")

#1. Merges the training and the test sets to create one data set.
X_Total <- rbind(X_Train, X_Test)
Y_Total <- rbind(Y_Train, Y_Test)
Subject_Total <- rbind(Subject_Train, Subject_Test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
Selected_Variable <- Variable_Names[grep("mean\\(\\)|std\\(\\)",Variable_Names[,2]),]
X_Total <- X_Total[,Selected_Variable[,1]]

# 3. Uses descriptive activity names to name the activities in the data set
colnames(Y_Total) <- "activity"
Y_Total$ActivityLabel <- factor(Y_Total$activity, labels = as.character(Activity_Labels[,2]))
ActivityLabel <- Y_Total[,-1]

# 4. Appropriately labels the data set with descriptive variable names.
colnames(X_Total) <- Variable_Names[Selected_Variable[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(Subject_Total) <- "subject"
Total <- cbind(X_Total, ActivityLabel, Subject_Total)
Total_Mean <- Total %>%
      group_by(ActivityLabel, subject) %>%
      summarise_each(mean)

write.table(Total_Mean, file = "/Users/bryangoh/Desktop/Coursera/DataScienceusingRCourse3/GettingandCleaningDataCourseProject/TidyData.txt", row.names=FALSE, col.names=TRUE)


