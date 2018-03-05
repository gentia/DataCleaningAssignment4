## Convert data files
features <- read.csv("./UCI HAR Dataset/features.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE) ## reading the feature file
final_features <- make.names(features[,2], unique = TRUE)## narrowing down the data to a list of unique features 
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_activity <- read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = " ")
train_subject <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = " ")

test_x <- read.table("./UCI HAR Dataset/test/x_test.txt")
test_activity <- read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = " ")
test_subject <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = " ")

# Create data table of test and train and labels dataset with descriptive variable names
train_data <- data.frame(train_subject, train_activity, train_x)
colnames(train_data) <- c(c("subject", "activity"), final_features)
test_data <- data.frame(test_subject, test_activity, test_x)
colnames(test_data) <- c(c("subject", "activity"), final_features)

# Merging the two tables
total_data <- rbind(test_data, train_data)
total_data <- arrange(total_data, subject, activity)

#Isolating the columns with only means and standard deviations
mean_std_data <- total_data[, grepl("mean|std", names(total_data))]

## Creating descriptive names for the activities 
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(activity_labels) <- c("activity", "ActivityName")
total_data <- merge(activity_labels,total_data, by = "activity")
total_data$activity <- NULL

new_names <- gsub("^t", "Time.", final_features)# replacing t with Time
new_names <- gsub("^f","Frequency.", new_names)# replacing f with Frenquency
new_names <- gsub("\\.{2,}", "\\.", new_names)# replace more than 1 period with a single period

##Function to capitalizing the first letter of all words in each string
simpleCap <- function(new_names)
{
  s <- strsplit(new_names,"\\.")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s,2), sep = "", collapse = ".")
}

new_names <- as.character(lapply(new_names, simpleCap))
colnames(total_data) <- c(c("ActivityName", "Subject"), new_names)

##independent tidy data set with the average of each variable for each activity and each subject.
new_tidy_data <- aggregate(total_data[,3:ncol(total_data)], by = list( ActivityName = total_data$ActivityName, Subject = total_data$Subject), FUN = mean)
write.table(new_tidy_data,file = "tidy_data.txt", row.names = FALSE)






  


























