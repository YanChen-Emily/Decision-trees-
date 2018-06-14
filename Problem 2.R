# problem 2
library(rpart)
#import data
housetype_data<-read.csv('housetype_stats315B.csv',header=TRUE)

#column names of age_data variables and their types
#all of them are integers
sapply(housetype_data,class)

library(dplyr)
mutate(housetype_data,
       TypeHome=as.factor(TypeHome),
       sex=as.factor(sex),
       MarStat=as.factor(MarStat),
       age=factor(age,ordered=TRUE), #age as ordinal factor
       Edu=factor(Edu,ordered=TRUE), #Edu as ordinal factor
       Occup=as.factor(Occup),
       Income=factor(Income,ordered=TRUE), #Income as ordinal factor
       LiveBA=factor(LiveBA,ordered=TRUE), #LiveBA as ordinal factor
       DualInc=as.factor(DualInc),
       HouseStat=as.factor(HouseStat),
       Ethnic=as.factor(Ethnic),
       Lang=as.factor(Lang)
)

#randomly select 90% of the data set as the training sample
train<-sample(1:nrow(housetype_data),0.9*nrow(housetype_data))
inputData<-housetype_data[train, ] #training data
testData<-housetype_data[-train, ] #test data

housetype.tree=rpart(TypeHome~.,data=inputData,method="class")

print(housetype.tree)
summary(housetype.tree)
printcp(housetype.tree)

library(rpart.plot)
rpart.plot(housetype.tree)

out<-predict(housetype.tree) #predict probabilities on the training data set

pred.response<-colnames(out)[max.col(out, ties.method=c("random"))] #predict response

mean(inputData$TypeHome !=pred.response) # misclassification error
# [1] 0.2635927

# predict probabilities on the test set
out_test<-predict(housetype.tree,testData) 

#prediction on the test data set
test.response<-colnames(out_test)[max.col(out_test,ties.method=c("random"))]

#misclassification error on the test data set
mean(testData$TypeHome !=test.response)

housetype.tree=rpart(TypeHome~.,data=housetype_data,method="class")
rpart.plot(housetype.tree)

out<-predict(housetype.tree) #predict probabilities on the training data set

pred.response<-colnames(out)[max.col(out, ties.method=c("random"))] #predict response

mean(housetype_data$TypeHome !=pred.response) # misclassification error
