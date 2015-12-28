#Load libraries and data:
library(caret);library(ggplot2);library(MASS);library(rattle);library(randomForest)
dat<-read.csv("training.csv")
colnames(dat)
str(dat)

#--------------------------Linear Discriminant Analysis--------------------------#
#Preprocess using PCA, use lda method, and train using cv::
set.seed(32343);training<-dat
modelFit<-train(classe~.,data=training,method="lda",preProcess="pca",
                trControl=trainControl(method = "cv"))

#Check In-Sample-Error rate:
trainPC<-predict(modelFit,training)
inSampleError<-confusionMatrix(trainPC,training$classe)
table(trainPC,training$classe)

#Predict using testing data:
testing<-read.csv("testing.csv");colnames(testing)
testPC<-predict(modelFit,testing[,-59])
summary(testPC)

#-------------------------------Random Forest--------------------------------#
Model<-randomForest(classe~.,method="rf",data=training,prox=TRUE,
             trControl=trainControl(method="oob",allowParallel=TRUE),ntree=100)
#Model details:
Model

#Predict new values:
testing<-read.csv("testing.csv");colnames(testing)
testPC<-predict(Model,testing[-53])
as.character(testPC)

#Submitting work:
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,
                            row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(testPC)
