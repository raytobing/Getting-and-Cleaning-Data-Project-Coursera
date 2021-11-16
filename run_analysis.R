# I. Setup and Download Data

library(rstudioapi)
library(data.table)
library(dplyr)
rootdir <- dirname(getSourceEditorContext()$path)
setwd(rootdir)

source <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(source, "./UCI_HAR.zip")
unzip("UCI_HAR.zip", exdir = getwd())

# II. Read Data

features <- read.csv('./UCI HAR Dataset/features.txt', 
                     header = FALSE, sep = ' ')
features <- as.character(features[,2])

data.train.x        <- read.table('./UCI HAR Dataset/train/X_train.txt')
data.train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', 
                                header = FALSE, sep = ' ')
data.train.subject  <- read.csv('./UCI HAR Dataset/train/subject_train.txt',
                                header = FALSE, sep = ' ')

data.train          <-  data.frame(data.train.subject, 
                                 data.train.activity, data.train.x)
names(data.train)   <- c(c('subject', 'activity'), features)

data.test.x         <- read.table('./UCI HAR Dataset/test/X_test.txt')
data.test.activity  <- read.csv('./UCI HAR Dataset/test/y_test.txt', 
                                header = FALSE, sep = ' ')
data.test.subject   <- read.csv('./UCI HAR Dataset/test/subject_test.txt', 
                                header = FALSE, sep = ' ')

data.test <-  data.frame(data.test.subject, data.test.activity, data.test.x)
names(data.test) <- c(c('subject', 'activity'), features)

# III. Data prep

#i. Merges the training and the test sets to create one data set.
data.all <- rbind(data.train, data.test)

#ii. Extracts only the measurements on the mean and std. dev. for each measurement.
mean_std.select <- grep('mean|std', features)
data.sub        <- data.all[,c(1,2,mean_std.select + 2)]

#iii. Uses descriptive activity names to name the activities in the data set
activity.labels   <- read.table('./UCI HAR Dataset/activity_labels.txt', 
                              header = FALSE)
activity.labels   <- as.character(activity.labels[,2])
data.sub$activity <- activity.labels[data.sub$activity]

#iv. Appropriately labels the data set with descriptive variable names.
name.new <- names(data.sub)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(data.sub) <- name.new

#v. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
data.tidy <- aggregate(data.sub[ ,3:81], by = list(activity = data.sub$activity, 
                                                  subject = data.sub$subject),FUN = mean)
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)