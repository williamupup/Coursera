#download and unzip the data into the selected directory
setwd("C:/Users/Haonan/Desktop/Study/R/getting data project")

#read activity labels and features
activity_labels <- read.table("activity_labels.txt")
activity_labels$V2 <- as.character(activity_labels$V2)
features <- read.table("features.txt")
features$V2 <- as.character(features$V2)

#this is to pick out all the features which contains "mean" or "std, and save the result to the variable "tomatch"
tomatch <- grep("mean|std", features$V2)

#read raw test set data, change the feature names, select needed data, combine raw data with activities and labels
tedata <- read.table("./test/X_test.txt")
colnames(tedata) <- features$V2
tedata <- tedata[tomatch]
teactivities <- read.table("./test/y_test.txt")
colnames(teactivities) <- "activities"
telabels <- read.table("./test/subject_test.txt")
colnames(telabels) <- "labels"
teset <- cbind(telabels,teactivities,tedata)

#read raw training set data, change the feature names, select needed data, combine raw data with activities and labels
trdata <- read.table("./train/X_train.txt")
colnames(trdata) <- features$V2
trdata <- trdata[tomatch]
tractivities <- read.table("./train/y_train.txt")
colnames(tractivities) <- "activities"
trlabels <- read.table("./train/subject_train.txt")
colnames(trlabels) <- "labels"
trset <- cbind(trlabels,tractivities,trdata)

totalset <- rbind(trset,teset)

#remove all the redundant variable, and combine the test data(teset) and training data(trset)
remove(teactivities,tedata,telabels,tractivities,trdata,trlabels,features)

#change labels and activities into factors, and then use "melt" to melt totalset by labels and activities, then use
#"dcast" to count the mean value by labels and activities
library(reshape2)
totalmelted <- melt(totalset, id = c("labels", "activities"))
totalmean <- dcast(totalmelted, labels + activities ~ variable, mean)

#I wrote a function to replace the number of activites into real characters, like 1 will be replaced with "WALKING"
#and then use sapply to run the function for activities one by one
act_replace <- function(labels) {
                  m <- labels
                  labels <- activity_labels[m,2]
         }
totalmean$activities <- sapply(totalmean$activities,act_replace)

#finally write the tidy data into the directory
write.table(totalmean,file = "tidy data.txt", row.names = F)
