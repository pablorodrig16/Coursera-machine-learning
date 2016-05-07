##First download training and test data files if they are not in the
##working directory

if(!file.exists("dataTraining.txt")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                "dataTraining.txt")
}

if(!file.exists("dataTest.txt")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                "dataTest.txt")
}

##Load training and test data

dataTraining<-read.csv("dataTraining.txt",header = TRUE)
dataTest<-read.csv("dataTest.txt", header = TRUE)

summary(dataTraining$classe)