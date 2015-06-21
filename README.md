This README file contains a description of the run_analysis.R script and the code book for the tidy data set generated by the run_analysis.R script.
====================================================================================================================================================

Script:
-------

This script follows the steps outlined in the Coursera Assignment, #1-5. 
 
STEP 1: Merges the training and the test sets to create one data set.

- The script imports the data for both the test and train data sets as separate data frames. It imports the raw data, which is formatted in a series of text files, and creates a data frame with a column for each variable from the text files for each data frame.
- The script creates the column labels for the X_test and X_train sets and labels the two data frames.
- The script checks that the headers are the same for both data frames for the test and train variable, and then merges the data into a new data frame called dat.


STEP 2: Extracts measurements on the mean and standard deviation for each measurement.

- The script uses grep functions to extract the indices of the columns that contain the fixed strings mean() and std().
- The script then combines the two grep functions into a single vector, sorts the indices from lowest to highest, and then adds 2 to each index - which accounts for the Subject and Activity columns when we apply this to our dat data frame.
- The script subsets our dat data frame into only the columns that contain means or std deviations, and then appends the Subject and Activity columns into the subset.
- The script also labels the columns with shortened descriptive variable names, which will be replaced with full descriptive variable names in STEP 4.

STEP 3: Uses descriptive activity names to name the activities in the data set.

- The script creates a vector with the activity labels, and then loops through the subset data to replace the number with the appropriate descriptive activity name.


STEP 4: Appropriately labels the data set with descriptive variable names. 

- In STEP 2, the variable names were shortened, so this step relabels the variables with the full names.

STEP 5: From the data in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

- The script loops through each column and takes the mean of that column by subject and activity. The loop builds a tidy data set, name by name, using the aggregate function.
- Then, the script writes the tidy data set to a file. 




Code Book:
----------

This code book is modified from the original code book, by Reyes-Ortiz, Anguita, Ghio and Oneto 2012, which is given as a blockquote. The code book for the data into a tidy data set is given below the quoted information.



>==================================================================
>Human Activity Recognition Using Smartphones Dataset
>Version 1.0
>==================================================================
>Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
>Smartlab - Non Linear Complex Systems Laboratory
>DITEN - Universit‡ degli Studi di Genova.
>Via Opera Pia 11A, I-16145, Genoa, Italy.
>activityrecognition@smartlab.ws
>www.smartlab.ws
>==================================================================
>
>The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration >and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
>
>The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. >The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
>
>
>Feature Selection 
>=================
>
>The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. >Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
>
>Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
>
>Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
>
>These signals were used to estimate variables of the feature vector for each pattern:  
>'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
>
>tBodyAcc-XYZ
>tGravityAcc-XYZ
>tBodyAccJerk-XYZ
>tBodyGyro-XYZ
>tBodyGyroJerk-XYZ
>tBodyAccMag
>tGravityAccMag
>tBodyAccJerkMag
>tBodyGyroMag
>tBodyGyroJerkMag
>fBodyAcc-XYZ
>fBodyAccJerk-XYZ
>fBodyGyro-XYZ
>fBodyAccMag
>fBodyAccJerkMag
>fBodyGyroMag
>fBodyGyroJerkMag
>
>The set of variables that were estimated from these signals are: 
>
>mean(): Mean value
>std(): Standard deviation
>
...
>
>
>Notes: 
>======
>- Features are normalized and bounded within [-1,1].
>- Each feature vector is a row on the text file.
>
>For more information about this dataset contact: activityrecognition@smartlab.ws
>
>License:
>========
>Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
>
>[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
>
>This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
>
>Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.




Tidy Data Set:
--------------

- The variables are denoted with the description of the type of motion (above), with the 3-axial signals in the X, Y and Z directions, as well as the mean or standard deviation (std) calculation.
- Means and standard deviations are calculated **by subject, by type of activity**, so that for any given subject and activity, there is a single calculation.
- for all variables except Subject and Activity, the data is normalized (i.e. unit-free).

The complete list of variables in the tidy data set: 
----------------------------------------------------

Subject: Participant identifier, in integers 1-30.                   
Activity: Activity performed, one of WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING  
tBodyAcc-mean()-X          
tBodyAcc-mean()-Y           
tBodyAcc-mean()-Z          
tBodyAcc-std()-X            
tBodyAcc-std()-Y            
tBodyAcc-std()-Z            
tGravityAcc-mean()-X        
tGravityAcc-mean()-Y       
tGravityAcc-mean()-Z        
tGravityAcc-std()-X        
tGravityAcc-std()-Y        
tGravityAcc-std()-Z     
tBodyAccJerk-mean()-X      
tBodyAccJerk-mean()-Y       
tBodyAccJerk-mean()-Z       
tBodyAccJerk-std()-X        
tBodyAccJerk-std()-Y        
tBodyAccJerk-std()-Z       
tBodyGyro-mean()-X          
tBodyGyro-mean()-Y          
tBodyGyro-mean()-Z          
tBodyGyro-std()-X           
tBodyGyro-std()-Y          
tBodyGyro-std()-Z           
tBodyGyroJerk-mean()-X      
tBodyGyroJerk-mean()-Y      
tBodyGyroJerk-mean()-Z      
tBodyGyroJerk-std()-X      
31 tBodyGyroJerk-std()-Y      
tBodyGyroJerk-std()-Z       
tBodyAccMag-mean()         
tBodyAccMag-std()           
tGravityAccMag-mean()      
tGravityAccMag-std()        
tBodyAccJerkMag-mean()      
tBodyAccJerkMag-std()      
tBodyGyroMag-mean()        
tBodyGyroMag-std()         
tBodyGyroJerkMag-mean()     
tBodyGyroJerkMag-std()      
fBodyAcc-mean()-X           
fBodyAcc-mean()-Y          
fBodyAcc-mean()-Z          
fBodyAcc-std()-X            
fBodyAcc-std()-Y            
fBodyAcc-std()-Z           
fBodyAccJerk-mean()-X       
fBodyAccJerk-mean()-Y      
fBodyAccJerk-mean()-Z      
fBodyAccJerk-std()-X       
fBodyAccJerk-std()-Y       
fBodyAccJerk-std()-Z        
fBodyGyro-mean()-X         
fBodyGyro-mean()-Y          
fBodyGyro-mean()-Z         
fBodyGyro-std()-X        
fBodyGyro-std()-Y           
fBodyGyro-std()-Z          
fBodyAccMag-mean()       
fBodyAccMag-std()          
fBodyBodyAccJerkMag-mean() 
fBodyBodyAccJerkMag-std()   
fBodyBodyGyroMag-mean()    
fBodyBodyGyroMag-std()    
fBodyBodyGyroJerkMag-mean() 
fBodyBodyGyroJerkMag-std() 