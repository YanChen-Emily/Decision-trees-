#Problem 1
library(rpart)

#import the data sets
age_data<-read.csv('age_stats315B.csv',header=TRUE)

#column names of age_data variables and their types
#all of them are integers
sapply(age_data,class)

#Make the variables as categorical variables
library(dplyr)
mutate(age_data,
          age=factor(age,ordered=TRUE), #age as ordinal factor
          Occup=as.factor(Occup),
          TypeHome=as.factor(TypeHome),
          sex=as.factor(sex),
          MarStat=as.factor(MarStat),
          Edu=factor(Edu,ordered=TRUE), #Edu as ordinal factor
          Income=factor(Income,ordered=TRUE), #Income as ordinal factor
          LiveBA=factor(LiveBA,ordered=TRUE), #LiveBA as ordinal factor
          DualInc=as.factor(DualInc),
          HouseStat=as.factor(HouseStat),
          Ethnic=as.factor(Ethnic),
          Lang=as.factor(Lang)
)

age.tree=rpart(age~.,data=age_data,method="class")
printcp(age.tree)
summary(age.tree)

library(rpart.plot)
rpart.plot(age.tree)

new.obs=c(NA,6,1,2,5,6,1,2,1,1,0,2,2,1)
age_data[nrow(age_data)+1, ]=new.obs
age_data[nrow(age_data), ]
out<-predict(age.tree,age_data[nrow(age_data), ])
colnames(out)[max.col(out, ties.method=c("random"))] 
#[1] "2"
# Bin 2 is for age 18 thru 24. That's the truth!


#randomly select the training sample
train<-sample(1:nrow(age_data),0.8*nrow(age_data))
inputData<-age_data[train, ] #training data
testData<-age_data[-train, ] #test data

#age.tree=rpart(age~.,data=inputData,method="class")

out<-predict(age.tree) #predict probabilities on the training data set

pred.response<-colnames(out)[max.col(out, ties.method=c("random"))] #predict response

mean(inputData$age !=pred.response) # misclassification error
#[1] 0.4804822

# predict probabilities on the test set
out_test<-predict(age.tree,testData) 

#prediction on the test data set
test.response<-colnames(out_test)[max.col(out_test,ties.method=c("random"))]

#misclassification error on the test data set
mean(testData$age !=test.response)
#[1] 0.4764638







      
