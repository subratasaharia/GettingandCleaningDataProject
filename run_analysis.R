## Run_analysis

library(dplyr)
library(stringr)
## Import data into R

features<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/features.txt",header = FALSE)
activity_labels<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/activity_labels.txt",header = FALSE)
subject_train<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/train/subject_train.txt",header = FALSE)
subject_test<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/test/subject_test.txt",header = FALSE)
X_test<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/test/X_test.txt",header = FALSE)
y_test<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/test/y_test.txt",header = FALSE)
y_train<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/train/y_train.txt",header = FALSE)
X_train<-read.table("./Coursera 3/Week 4/PA/UCI HAR Dataset/train/X_train.txt",header = FALSE)

# Cleaning varibale names
variablenames<-gsub(",","_",features$V2)
variablenames<-gsub("-","_",variablenames)

# Adding legible headers to train data set
names(X_train)<-variablenames
names(subject_train)<-c("subject_id")
names(y_train)<-c("activitylabels")
Traindata<-data.frame(X_train,subject_train,y_train)

# Adding legible headers to test data set
names(X_test)<-variablenames
names(subject_test)<-c("subject_id")
names(y_test)<-c("activitylabels")
Testdata<-data.frame(X_test,subject_test,y_test)

# Convert data table to data frame for extracting means and std columns
Combined_dataframe<-rbind(as.data.frame(Traindata),as.data.frame(Testdata))
MeanStdExtract<-data.frame(Combined_dataframe[,grep("std",names(Combined_dataframe),value=TRUE)],Combined_dataframe[,grep("mean",names(Combined_dataframe),value=TRUE)],Combined_dataframe$subject_id,Combined_dataframe$activitylabels)
names(MeanStdExtract)<-c(grep("std",names(Combined_dataframe),value=TRUE),grep("mean",names(Combined_dataframe),value=TRUE),"subject_id","activitylabels")

# Naming activities in the dataset
for( i in 1:nrow(MeanStdExtract)){
  if( (MeanStdExtract$activitylabels[i])=="1")
    MeanStdExtract$activitylabels[i]<-c("WALKING")
  if( (MeanStdExtract$activitylabels[i])=="2")
    MeanStdExtract$activitylabels[i]<-c("WALKING_UPSTAIRS")
  if( (MeanStdExtract$activitylabels[i])=="3")
    MeanStdExtract$activitylabels[i]<-c("Walking_DOWNSTAIRS")
  if( (MeanStdExtract$activitylabels[i])=="4")
    MeanStdExtract$activitylabels[i]<-c("SITTING")
  if( (MeanStdExtract$activitylabels[i])=="5")
    MeanStdExtract$activitylabels[i]<-c("STANDING")
  if((MeanStdExtract$activitylabels[i])=="6")
    MeanStdExtract$activitylabels[i]<-c("LAYING")
}

# Creating new data set
SubjectMeanStdbyActivity<-MeanStdExtract[,1:79] %>% mutate(newfactor=as.factor(paste(MeanStdExtract$subject_id,MeanStdExtract$activitylabels))) %>% group_by(newfactor) %>% summarise_each(funs(mean(.,na.rm=TRUE)))
names(SubjectMeanStdbyActivity)<-c("Subject_Activity", grep("std",features$V2,value=TRUE),grep("mean",features$V2,value=TRUE))

SubjectMeanStdbyActivity<-SubjectMeanStdbyActivity %>% mutate(Subject= str_trim(substr(SubjectMeanStdbyActivity$Subject_Activity,1,2))) %>%mutate(Activity= str_trim(substr(SubjectMeanStdbyActivity$Subject_Activity,3,length(SubjectMeanStdbyActivity$Subject_Activity))))
SubjectMeanStdbyActivity<-SubjectMeanStdbyActivity[order(as.numeric(SubjectMeanStdbyActivity$Subject)),]
# realigning data frame to start with columns subject and activity
Move<-select(SubjectMeanStdbyActivity,81:82)
SubjectMeanStdbyActivity<-cbind(Move,SubjectMeanStdbyActivity[,2:80])
# Saving the tidy data set
write.table(SubjectMeanStdbyActivity,"./Coursera 3/Week 4/PA/SubjectMeanStdbyActivity.txt",row.names= FALSE)
