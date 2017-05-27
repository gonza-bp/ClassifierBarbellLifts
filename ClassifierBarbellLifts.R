training<-read.csv("pml-training.csv",na.strings = c("NA","NaN","","#DIV/0!"))
testing<-read.csv("pml-testing.csv",na.strings = c("NA","NaN","","#DIV/0!"))

removeCols<-colSums(is.na(training))>0
removeCols<-names(removeCols[removeCols])
removeCols<-c(removeCols,'X','user_name')
removeCols<-c(removeCols,'raw_timestamp_part_1','raw_timestamp_part_2')
removeCols<-c(removeCols,'cvtd_timestamp','new_window','num_window')
training<-training[,setdiff(names(training),removeCols)]
testing<-testing[,setdiff(names(testing),removeCols)]

library(caret)
# define training control
train_control <- trainControl(method="cv", number=4)
modelLDA<-train(classe~.,data=training,trainControl="train_control",method="lda")
print(modelLDA)
confusionMatrix(modelLDA)
preds<-predict(modelLDA,testing)

modelNB<-train(classe~.,data=training,method="nb")
print(modelNB)
confusionMatrix(modelNB)
preds<-predict(modelNB,testing)