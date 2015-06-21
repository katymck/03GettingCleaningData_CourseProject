# This script follows the steps outlined in the Coursera Assignment, #1-5. 
# 
# STEP 1: Merges the training and the test sets to create one data set.

#IMPORTING THE DATA:
#This creates the column labels for the X_test and X_train sets.
setwd("~/Documents/Katy's Documents/Coursera/Data Science Specialization/03 Getting & Cleaning Data/Course Project/UCI HAR Dataset")
X_test_labs<-read.table(file="features.txt")
X_test_labs<-X_test_labs[-1]


#This imports the test data set.
setwd("~/Documents/Katy's Documents/Coursera/Data Science Specialization/03 Getting & Cleaning Data/Course Project/UCI HAR Dataset/test")
subject_test<-read.table(file="subject_test.txt", col.names="Subject")
X_test<-read.table(file="X_test.txt", col.names=X_test_labs$V2)
y_test<-read.table(file="y_test.txt", col.names="Activity")

dat_test<-cbind(subject_test,y_test,X_test)


#This imports the train data set.
setwd("~/Documents/Katy's Documents/Coursera/Data Science Specialization/03 Getting & Cleaning Data/Course Project/UCI HAR Dataset/train")
subject_train<-read.table(file="subject_train.txt", col.names="Subject")
X_train<-read.table(file="X_train.txt", col.names=X_test_labs$V2)
y_train<-read.table(file="y_train.txt", col.names="Activity")

dat_train<-cbind(subject_train,y_train,X_train)



#STEP 2: Merge training and test data sets.

#This checks that the headers are the same for both data frames for the "test" and "train" variable, and then merges the data into a new data frame called "dat".
names(dat_test)==names(dat_train)
dat<-rbind(dat_test,dat_train)


#STEP 3: Extracts measurements on the mean and standard deviation for each measurement.

#The grep functions extract the indices of the columns that contain the fixed strings "mean()" and "std()".
grepmean<-grep("mean()",X_test_labs$V2,fixed=TRUE)
grepstd<-grep("std()",X_test_labs$V2,fixed=TRUE)

#This combines the two grep functions into a single vector, storts it, and then adds 2 to each index - which accounts for the Subject and Activity columns when we apply this to our "dat" data frame.
mean_std<-c(grepmean,grepstd)
mean_std2<-sort(mean_std)
mean_std3<-mean_std2+2

#This subsets our "dat" data frame into only the columns that contain means or std deviations.
datsub<-dat[,mean_std3]

#This appends the Subject and Activity columns into our subset.
datsuball<-cbind(dat$Subject,dat$Activity,datsub)

#This renames the columns to the "features.txt" names - though in Step 4 I will rename them with full names rather than the shortened version here.
library(plyr)
datsuball2<-rename(datsuball,c("dat$Subject"="Subject","dat$Activity"="Activity"))


#STEP 3: Uses descriptive activity names to name the activities in the data set.

#This creates a vector with the activity labels, and then loops through the subset data to replace the number with the appropriate descriptive activity name.
Activity_labels<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
for (i in 1:6){
datsuball2$Activity[datsuball2$Activity==i] <- Activity_labels[i]
}


#STEP 4: Appropriately labels the data set with descriptive variable names. 
#This was done in step 2, so they are already labeled. But the names are shortened due to make.names, so this relabels the variables with the full names.

newnames<-c("Subject","Activity",as.character(X_test_labs$V2[mean_std2]))

colnames(datsuball2) <- newnames


#STEP 5: From the data in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#This function loops through each column, and takes the mean of the column. The loop builds a tidy data set, name by name, using the aggregate function.

names<-names(datsuball2)
names2<-names[-(1:2)]

for (name in names2){
    
  #if it exists, take the mean of the current column in the loop.
  if (exists("tidydat")){
    means<-aggregate(datsuball2[,name] ~ Subject+Activity, datsuball2, mean)
    tidydat<-cbind(tidydat,means[,3])
  }
  
    #if it doesn't exist, create it and append Subject & Activity:
  if (!exists("tidydat")){
    tidydat<-data.frame()
    means<-aggregate(datsuball2[,name] ~ Subject+Activity, datsuball2, mean)
    tidydat<-means
  }
}

#This renames the columns, once again using the descriptive variable names, this time for the tidy data set. 
newnames<-c("Subject","Activity",as.character(X_test_labs$V2[mean_std2]))
colnames(tidydat)<-newnames



#This saves the tidy data set to a .txt file.
write.table(tidydat,file="tidydata.txt",row.names=FALSE)


