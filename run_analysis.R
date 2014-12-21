# This function is to download, unzip and read in the UCI HAR Dataset, combine all the data,
# create a subset of data containing the measurements on the mean and standard deviation for
# each measurement, and generate a new tidy dataset with appropriate labeling in txt format. 
run_analysis<-function(){
                # Download and unzip the UCI HAR Dataset if not found in working directory
                if (!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
                                url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                                download.file(url,destfile="getdata-projectfiles-UCI HAR Dataset.zip",method="curl")
                }
                unzip("getdata-projectfiles-UCI HAR Dataset.zip",exdir=".")
                # Read in the unzipped dataset, skipping Inertial Signals folder
                X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
                y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
                subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
                X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
                y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
                subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
                # Combine all the data from test and train into one big dataset
                test<-cbind(subject_test,y_test,X_test)
                train<-cbind(subject_train,y_train,X_train)
                data<-rbind(train,test)
                # Subset the dataset by extracting only the measurements on the mean and
                # standard deviation (all the measurements containing the keyword)
                # Label each column in the extracted dataset with corresponding names
                names<-read.table("./UCI HAR Dataset/features.txt")
                names<-cbind("subject","y_parameter",t(names))
                column_name<-make.names(names[2,])
                sub<-grepl("subject|y_parameter|[Mm]ean|[Ss]td",column_name)
                data_sub<-data[sub]
                colnames(data_sub)<-column_name[sub]
                # Use descriptive activity names to replace the original numbering system
                data_sub$y_parameter<-gsub(1,"Walking",data_sub$y_parameter)
                data_sub$y_parameter<-gsub(2,"Walking_upstairs",data_sub$y_parameter)
                data_sub$y_parameter<-gsub(3,"Walking_downstairs",data_sub$y_parameter)
                data_sub$y_parameter<-gsub(4,"Sitting",data_sub$y_parameter)
                data_sub$y_parameter<-gsub(5,"Standing",data_sub$y_parameter)
                data_sub$y_parameter<-gsub(6,"Laying",data_sub$y_parameter)
                # Create a new tidy data set with the average of each variable for each activity and subject
                tidydata<-data.frame()
                for (i in 1:30){
                                for (j in c("Walking","Walking_upstairs","Walking_downstairs","Sitting","Standing","Laying")){
                                                temp<-data_sub[(data_sub$subject==i&data_sub$y_parameter==j),3:88]
                                                means<-colMeans(temp)
                                                features<-column_name[sub][3:88]
                                                activity<-rep(j,times=86)
                                                subject<-rep(i,times=86)
                                                tidydata<-rbind(tidydata,cbind(subject,activity,features,means))
                                }               
                }
                # Label the new tidy dataset with appropriate column and row names
                colnames(tidydata)<-c("Subject","Activity","Feature","Mean")
                rownames(tidydata)<-1:nrow(tidydata)
                # Create a txt file of the tidy dataset in the working directory
                write.table(tidydata,file="tidydataset.txt",row.name=FALSE)
}