
library(plyr)

file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data_path <- "UCI HAR Dataset"
result_folder <- "results"

## Create data and folders   
# verifies the data zip file has been downloaded
if(!file.exists(file)){
  
  ##Downloads the data file
  print("downloading Data")
  download.file(url,file, mode = "wb")
  
}

if(!file.exists(result_folder)){
  print("Creating result folder")
  dir.create(result_folder)
} 


## Read the table from the zip data file
getTable <- function (filename,cols = NULL){
  
  f <- unz(file, paste(data_path,filename,sep="/"))
  
  data <- data.frame()
  
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  
  
  data
  
}

# Read Data and create data set
getData <- function(type, features){
  
  subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
  x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
  
  return (cbind(subject_data,y_data,x_data)) 
}

# Saves final product to the results folder
saveResult <- function (data,name){
  
  file <- paste(result_folder, "/", name,".csv" ,sep="")
  write.csv(data,file)
}

# Use Features.txt to get common data tables for column names
features <- getTable("features.txt")

# Load the two data sets
train <- getData("train",features)
test <- getData("test",features)

### Question 1: Merges the training and the test sets to create one data set.

data <- rbind(train, test)

# rearrange the data using id
data <- arrange(data, id)

### Question 3: Uses descriptive activity names to name the activities in the data set
### Question 4: Appropriately labels the data set with descriptive variable names. 
activity_labels <- getTable("activity_labels.txt")
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

### Question 2: Extracts only the measurements on the mean and standard deviation for each measurement.  
kdata1 <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]

# Save kdata1 into results folder
saveResult(kdata1,"kdata1")

### Question 5: From the data set in step 4, creates a second, independent tidy data set 
###### with the average of each variable for each activity and each subject. 
tidyData <- ddply(kdata1, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

# MOdify columns to a
colnames(tidyData)[-c(1:2)] <- paste(colnames(tidyData)[-c(1:2)], "_MEAN", sep="")
# Save tidy dataset2 into results folder
saveResult(tidyData,"tidyData")
