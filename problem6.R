rm(list=ls())
library(gbm)
spam_train<-read.table('spam_stats315B_train.csv',sep=',')
spam_test<-read.table('spam_stats315B_test.csv',sep=',')

rflabs<-c("make", "address", "all", "3d", "our", "over", "remove",
          "internet","order", "mail", "receive", "will",
          "people", "report", "addresses","free", "business",
          "email", "you", "credit", "your", "font","000","money",
          "hp", "hpl", "george", "650", "lab", "labs",
          "telnet", "857", "data", "415", "85", "technology", "1999",
          "parts","pm", "direct", "cs", "meeting", "original", "project",
          "re","edu", "table", "conference", ";", "(", "[", "!", "$", "#",
          "CAPAVE", "CAPMAX", "CAPTOT","type")
# Names for predictors and response

colnames(spam_train)<-rflabs
colnames(spam_test)<-rflabs

#call the gbm function to train the Gradient Boosting Model
set.seed(130)
gbm0<-gbm(type~.,data=spam_train, train.fraction=0.8,
          interaction.depth=4, shrinkage=0.05,
          n.trees=2500, bag.fraction=0.5, cv.folds=5,
          distribution="bernoulli", verbose=T)

# optimal number of iterations 
best.iter_test<-gbm.perf(gbm0, method="cv")
## 952

# prediction on the training set 
gbm0.predict<-predict(gbm0,spam_train,type="response",n.trees=best.iter_test)

# set the threshold to the predicted value
gbm0.predict[gbm0.predict<0.5]=0
gbm0.predict[gbm0.predict>=0.5]=1

matrix.train<-table(gbm0.predict, spam_train$type)

# Training error: 0.008477339
(matrix.train[1,2]+matrix.train[2,1])/nrow(spam_train)

# non-spam error: 0.004326663
matrix.train[2,1]/(matrix.train[1,1]+matrix.train[2,1])

# spam error: 0.01477833
matrix.train[1,2]/(matrix.train[1,2]+matrix.train[2,2])

# prediction on the test set
gbm0.predict.test<-predict(gbm0, spam_test, type="response",n.trees=best.iter_test)

# still use the 0.5 threshold for prediction
gbm0.predict.test[gbm0.predict.test<0.5]=0
gbm0.predict.test[gbm0.predict.test>=0.5]=1

matrix.test<-table(gbm0.predict.test,spam_test$type)
# test_error: 0.02411995
test_error<-(matrix.test[1,2]+matrix.test[2,1])/nrow(spam_test)  


# still use the 0.5 threshold for prediction
gbm0.predict.test[gbm0.predict.test<0.5]=0
gbm0.predict.test[gbm0.predict.test>=0.5]=1

matrix.test<-table(gbm0.predict.test,spam_test$type) 
# test_error: 0.02411995
(matrix.test[1,2]+matrix.test[2,1])/nrow(spam_test)  
# non_spam test error: 0.02183406
matrix.test[2,1]/(matrix.test[2,1]+matrix.test[1,1]) 
# spam test error: 0.02750809
matrix.test[1,2]/(matrix.test[1,2]+matrix.test[2,2]) 

# adjust the loss weights 
weights<-rep(1,nrow(spam_train))
weights[spam_train$type==0]=55
mod.gbm<-gbm(type~., data=spam_train, weights=weights,
             interaction.depth = 4,shrinkage=0.05, n.trees=2500,train.fraction=0.8, 
             bag.fraction = 0.5, cv.folds=5, distribution='bernoulli', verbose=T)
best.iter_train<-gbm.perf(mod.gbm, method="cv")
gbm2.predict.test<-predict(mod.gbm, spam_test, type="response", n.trees=best.iter_train)
gbm2.predict.test[gbm2.predict.test < 0.935]= 0
gbm2.predict.test[gbm2.predict.test >= 0.935]= 1
gbm2.matrix.test<-table(gbm2.predict.test, spam_test$type)


# overall test error: 
(gbm2.matrix.test[1,2]+gbm2.matrix.test[2,1])/nrow(spam_test)
# test error of non-spam:
gbm2.matrix.test[2,1]/(gbm2.matrix.test[1,1]+gbm2.matrix.test[2,1])
# test error of spam:
gbm2.matrix.test[1,2]/(gbm2.matrix.test[1,2]+gbm2.matrix.test[2,2])

#import predictors:
summary(mod.gbm, n.tree = best.iter_train, main = "RELATIVE INFLUENCE OF ALL PREDICTORS")

names(spam_test)

#remove 
plot(x=mod.gbm, i.var=7, n.tree=best.iter_train, main="Partial Dependence of 'remove'")

# 000
plot(x=mod.gbm, i.var=23, n.tree=best.iter_train, main="Partial Dependence of 'remove'")

# !
plot(x=mod.gbm, i.var=52, n.tree=best.iter_train, main="Partial Dependence of 'remove'")

# partial dependence on remove and 000
plot(x=mod.gbm, i.var=c(7,23), n.trees=best.iter_train,main = "Partial Dependence on '!' and 'remove'")

# partial dependence on remove and !
plot(x=mod.gbm, i.var=c(7,52), n.trees=best.iter_train,main = "Partial Dependence on '!' and 'remove'")

# partial dependence on ! and 000
plot(x=mod.gbm, i.var=c(52,23), n.trees=best.iter_train,main = "Partial Dependence on '!' and 'remove'")



