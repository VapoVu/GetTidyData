library(plyr, reshape2)

# Function to check features are mean() and std() only
checkStats <- function(name){
        chk <- grepl("mean\\(\\)", name) | grepl("std\\(\\)", name)
        chk        
}

# Function to concatnate training and testing data

concatData <- function (dataset, fileDir){
        trainFile = paste(fileDir, "/train/", dataset, "_train.txt", sep = "")
        dataTrain <- read.table(trainFile)
        
        testFile = paste(fileDir, "/test/", dataset, "_test.txt", sep = "")
        dataTest <- read.table(testFile)
        
        finalData <- rbind(dataTrain, dataTest)
        
        finalData        
}

# Merging the training and testing datasets

homeDir = '/home/vapo/coursera/Getting and Cleaning Data/UCI HAR Dataset'

setwd(homeDir)

# Reading general infos
features <- read.table('features.txt', col.names = c('FeatureID', 'Feature'), 
                       colClasses = c("numeric", "character"))

labels <- read.table('activity_labels.txt', col.names = c('ActivityID', 'ActivityLabel'))

# Select mean() and std() features only
selFeatures <- features[sapply(features$Feature, checkStats), ]

# Reading, merging traing and testing datasets

subject = concatData("subject", homeDir)
names(subject) <- c("Volunteer")

Xdata = concatData("X", homeDir)

Ydata = concatData("y", homeDir)
names(Ydata) <- c("ActivityID")

# Select measurements of mean() and std() features only
Xsel <- Xdata[,c(selFeatures$FeatureID)]

names(Xsel) <- c(selFeatures$Feature)

# Combining into tidy dataset and write to text file
mergeData <- as.data.frame(cbind(subject, Ydata, Xsel))

mergeData <- join(mergeData, labels, by = c("ActivityID"))

write.table(mergeData, "tidyData.txt", col.names = T)

# Romove activity ID column and sort dataset by Volunteer and Activity Label
mergeData <- mergeData[, colnames(mergeData)!="ActivityID"]

mergeData <- arrange(mergeData, Volunteer, ActivityLabel)

# Melting data to compute aggregate statistics
meltData <- melt(mergeData, id = c("Volunteer", "ActivityLabel"), measure.vars = c(selFeatures$Feature))

finFeatMean <- dcast(meltData, Volunteer + ActivityLabel ~ variable, mean)

write.table(finFeatMean, "AverageTidyData.txt", col.names = T, row.names = F)

