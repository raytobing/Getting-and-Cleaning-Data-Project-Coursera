# I. Setup and Download Data

library(rstudioapi)
library(data.table)
library(dplyr)
rootdir <- dirname(getSourceEditorContext()$path)
setwd(rootdir)

source <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(source, "./UCI_HAR.zip")
unzip("UCI_HAR.zip", exdir = getwd())

# II. Reading the data

#i) Training Set

train_subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',
                          header = FALSE, sep = ' ')
train_label   <- read.csv('./UCI HAR Dataset/train/y_train.txt', 
                          header = FALSE, sep = ' ')
train_set     <- read.table('./UCI HAR Dataset/train/X_train.txt')

train         <- data.frame(train_subject, train_label, train_set)

#ii) Testing set

test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt',
                         header = FALSE, sep = ' ')
test_label   <- read.csv('./UCI HAR Dataset/test/y_test.txt', 
                         header = FALSE, sep = ' ')
test_set     <- read.table('./UCI HAR Dataset/test/X_test.txt')

test         <- data.frame(test_subject, test_label, test_set)

#iii) Variable naming

features  <- read.csv('./UCI HAR Dataset/features.txt', 
                     header = FALSE, sep = ' ')
var_names <- as.character(features[, 2]) 

names(train) <- c(c("subject", "label"), var_names)
names(test)  <- c(c("subject", "label"), var_names)

# III. Manipulation

#i) Merges the training and the test sets to create one data set.

combined_data   <- rbind(train, test)

#ii) Extracts only the measurements on the mean and sd for each measurement. 

mean_vars <- grep("mean", var_names)
sd_vars   <- grep("std",  var_names)

mean_data        <- combined_data[, c(mean_vars + 2)] # +2 in order to avoid taking the first two variables
sd_data          <- combined_data[, c(sd_vars + 2)]

subset_data      <- cbind(combined_data[, 1:2], mean_data, sd_data)

#iii) Uses descriptive activity names to name the activities in the data set

activity          <- read.table('./UCI HAR Dataset/activity_labels.txt', header = F)
activity_names    <- as.character(activity[, 2])
subset_data$label <- activity_names[subset_data$label]
subset_data       <- rename(subset_data, Activity = label)

#iv) Appropriately labels the data set with descriptive variable names. 

proper_label <- names(subset_data)

proper_label <- gsub("Acc", "Accelerometer", proper_label)
proper_label <- gsub("Gyro", "Gyroscope", proper_label)
proper_label <- gsub("Mag", "Magnitude", proper_label)

proper_label <- gsub("[(][)]", "", proper_label)
proper_label <- gsub("-mean-", "_Mean_", proper_label)
proper_label <- gsub("-std-", "_Standard_Deviation_", proper_label)

proper_label <- gsub("^t", "Time_", proper_label)
proper_label <- gsub("^f", "Frequency_", proper_label)

names(subset_data) <- proper_label

#v) Creates an independent tidy data set with the average of each variable for 
#   each activity and each subject.

independent <- aggregate(subset_data[ ,3:81], 
                         by = list(activity = subset_data$Activity, 
                                              subject = subset_data$subject), 
                                   FUN = mean)

write.table(x = independent, file = "independent.txt", row.names = FALSE)