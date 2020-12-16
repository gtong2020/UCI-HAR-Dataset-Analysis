if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")};library(tidyr)
#Step1
trainx <- read.table("UCI HAR Dataset//train/X_train.txt", nrows=7352, comment.char="")
trainsub <- read.table("UCI HAR Dataset//train/subject_train.txt", nrows=7352,col.names=c("subject"))
trainy <- read.table("UCI HAR Dataset/train//y_train.txt", nrows=7352,col.names=c("activity"))
traindata <- cbind(trainx, trainsub, trainy)
testx <- read.table("UCI HAR Dataset//test/X_test.txt", nrows=2947, comment.char="")
testsub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
testy <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity"))
testdata <- cbind(testx, testsub, testy)
data <- rbind(traindata, testdata)
#Step2
feature_list <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "name"))
features <- c(as.vector(feature_list[, "name"]), "subject", "activity")
filtered_feature_ids <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
filtered_data <- data[, filtered_feature_ids]

# Step3
activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names=c("id", "name"))
for (i in 1:nrow(activities)) {
  filtered_data$activity[filtered_data$activity == activities[i, "id"]] <- as.character(activities[i, "name"])
}
filtered_data
#step4
filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)
names(filtered_data) <- filtered_feature_names

#step5

tidy_data <- tbl_df(filtered_data) %>%
  group_by('subject', 'activity') %>%
  summarise_each(funs(mean)) %>%
  gather(measurement, mean, -activity, -subject)


write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
tidy_data
