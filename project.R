##Load required libraries

library (caret); library (ggplot2); library(rattle); library (MASS)

################################Getting data####
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

dataTraining<-read.csv("dataTraining.txt",header = TRUE,stringsAsFactors = FALSE,na.strings = c("","NA"))
dataTest<-read.csv("dataTest.txt", header = TRUE, stringsAsFactors = FALSE,na.strings = c("","NA"))


######################Data description############
##description of training data set
dim(dataTraining)
str(dataTraining)

qplot(data = dataTraining,x = classe)


########################Cleaning data##############
##Detecting variables with rate of missing data and selecting variables of data.frames
naVar<-apply(dataTraining,2, function (x) mean(is.na(x)))

dataTraining<-dataTraining[,naVar<0.5]
dataTest<-dataTest[,naVar<0.5]

##Subsetting interesting variables

dataTraining<-dataTraining[,8:ncol(dataTraining)]
dataTest<-dataTest[,8:ncol(dataTest)]

##########################Spliting data in training and test sets###############
##Split dataTraining in 2 training and test set for out of sample error estimation

trainingSubset<-createDataPartition(y = dataTraining$classe,p = .7,list = FALSE)

training<-dataTraining[trainingSubset,]
test<-dataTraining[-trainingSubset,]

##################Training data dimensions reduction########
##Reducing data dimensions from training dataset

##eliminates highly correlated variables (r>.9)
corMatrix<-cor(training[,-53])
highCor<-findCorrelation(corMatrix,cutoff = .9)

training<-training[,-highCor]
test<-test[,-highCor]

##PCA on training covariates
pca<-preProcess(training[,-46],method = c("center","scale","pca"),thresh = .8)

trainingPCA<-predict(pca, training[,-46])

trainingPCA<-cbind(trainingPCA,classe=training$classe)

qplot(data = trainingPCA,x = PC1,y = PC2,color=classe)


########################################### Modeling##################
###Modelling with caret::train function with 10 k-fold cross validation
tc<-trainControl(method = "cv",number = 10)

##LDA model
set.seed(1234)
modelLDA<-train(form = classe ~ .,
                data = training,
                trControl=tc,
                method="lda")

##QDA model
set.seed(1234)
modelQDA<-train(form = classe ~ .,
                data = training,
                trControl=tc,
                method="qda")

##NB model
set.seed(1234)
modelNB<-train(form = classe ~ .,
                data = training,
                trControl=tc,
                method="nb")

##Boosted tree model (gbm, gradient boosting machine)
set.seed(1234)
modelBoost<-train(form = classe ~ .,
                  data = training, 
                  trControl=tc, 
                  method="gbm",
                  verbose=FALSE)
##Random forest
set.seed(1234)
modelRF<-train(form = classe ~ .,
                  data = training, 
                  trControl=tc, 
                  method="rf",
                  verbose=FALSE)

######################Model selection####################
##Choose best model comparing metrics on cv
resamps <- resamples(list(LDA = modelLDA,
                          QDA = modelQDA,
                          NB = modelNB,
                          GBM = modelBoost,
                          RF= modelRF))

bwplot(resamps)


#####################testing RF model on test set##############
testPrediction<-predict(modelRF,test)

confusionMatrix(testPrediction,test$classe)


##################prediction of dataTest#####################

predict(modelRF, dataTest)