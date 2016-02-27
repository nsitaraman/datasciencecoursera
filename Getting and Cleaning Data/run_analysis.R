#load required packages for data manipulation
require(dplyr)
require(tidyr)

#For this script to run in the working directory the dataset directory UCI HAR Dataset should be located in the working directory.

#Load activity labels and name each variable in dataframe.
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE,
                              colClasses = c("numeric", "character"), col.names =  c("code", "activity"))

#Load subject train and test data and name column.
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subject")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE,  col.names = "subject")

#Load activity train and test data (y) and name column.
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = "activity")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = "activity")

#Use activity labels and factors to generate descriptive activity names for activity datasets.
activity_train$activity <- factor(activity_train$activity, labels = activity_labels$activity)
activity_test$activity <- factor(activity_test$activity, labels = activity_labels$activity)

#Load feature list and name columns.
feature_list <-  read.table("UCI HAR Dataset/features.txt", header = FALSE,
                              colClasses = c("numeric", "character"), col.names = c("index", "feature"))

#Load feature train and test data (x). Name columns with names supplied from feature list.
features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, 
                             colClasses = "numeric", col.names = feature_list$feature, check.names = FALSE)
features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, 
                            colClasses = "numeric", col.names = feature_list$feature, check.names = FALSE)

#Merge subject, feature and activity datasets
merged_train <- cbind(subject_train, features_train, activity_train)
merged_test <- cbind(subject_test, features_test, activity_test)

#Merge train and test data 
smartphones <- rbind(merged_test, merged_train)

#Use a regex to find only mean and std dev measurement indices from feature list
mean_std <- grepl("[Mm]ean|std", feature_list$feature)

#Use indices to extract measurements on mean and std dev for all observations,
#Make sure to include subject and activity variables too (first and last indices in smartphones dframe)
smartphones_filtered <- smartphones[ , c(TRUE, mean_std, TRUE)]

#The rest of data manipulation will use the pipe (%>%) operation to chain together multiple commands
# First the dataset is collapsed to long tidy data form by using gather with keys subject and activity (reshape2's melt() could also be used)
# Then the long data is grouped by activity, subject and variable.
# This then allows use of summarize to find the average of each variable for each activity and each subject. This is then our narrow tidy data.
tidy_data_narrow <- smartphones_filtered %>% gather(variable, value, -subject, -activity) %>%
                         group_by(activity, subject, variable) %>%
                         summarize(Mean = mean(value))

#Note that summarize has also ordered the data for us by factor (recall WALKING is actually 1 when unclassed), then subject and finally variable.

#We can also create the wide form of tidy data by using spread. Here each feature has its own coumn of mean values.
tidy_data_wide <- spread(tidy_data_narrow, variable, Mean)

#The version of tidy data to use depends on the situation and the data analysis to be performed. For this assignment the narrow form will be output to file.
write.table(tidy_data_narrow, "tidy_data_narrow.txt", row.name = FALSE)
