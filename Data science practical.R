#install needed packages
install.packages("e1071")
library("e1071")

#Read data from file 
data <- read.table("Documents/nbtrain.csv", sep = ",", header = TRUE)

#define 2 data frames one for train and another for test with sizes 9010, 1000
train <- as.data.frame(data[1:9010,])
test <- as.data.frame(data[9011:10010,])

########## Construct a Naïve Bayes classifier to predict income ############Part 1

#use naive bayes classifier with training data
model <- naiveBayes(income ~., train)
model

#score the model with the testing data
output <- predict(model,test)

#create the model's confusion matrix
confusion <- table(actual = test[,4],predicted = output)
confusion

#calculate the model accuracy
accuracy  <- sum(diag(confusion))/sum(confusion)
accuracy

############################################################################
########## Construct a Naïve Bayes classifier to predict gender ############Part 2

#use naive bayes classifier with training data
model1 <- naiveBayes(gender ~., train)
model1

#score the model with the testing data
output1 <- predict(model1,test)

#create the model's confusion matrix
confusion1 <- table(actual = test[,2],predicted = output1)
confusion1

#calculate the model accuracy
accuracy1  <- sum(diag(confusion1))/sum(confusion1)
accuracy1

############################################################################
## Construct a Naïve Bayes classifier to predict gender with balanced data ##Part 3

#Divide the training data into male & female
male <- subset (train, train$gender =='M')
female <- subset (train, train$gender =='F')

#select 3500 random records from each partition
mtrain <- male[sample(nrow(male), 3500),]
ftrain <- female[sample(nrow(female), 3500),]

#Merge them into new train data
newtrain <- rbind(mtrain,ftrain)

#use naive bayes classifier with new training data
model2 <- naiveBayes(gender ~., newtrain)
model2

#score the model with the testing data
output2 <- predict(model2,test)

#create the model's confusion matrix
confusion2 <- table(actual = test[,2],predicted = output2)
confusion2

#calculate the model accuracy
accuracy2  <- sum(diag(confusion2))/sum(confusion2)
accuracy2
