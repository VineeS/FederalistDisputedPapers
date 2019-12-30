# # LOAD Data SET
paper_dataset <- read.csv("fedPapers85.csv", header = TRUE)

#### remove all the data set that are disputed

fedDataSet_filtered <- paper_dataset [(!(paper_dataset$author=="dispt") & !(paper_dataset$author=="Jay") 
& !(paper_dataset$author=="HM")),]
fedDataSet_disputed<- paper_dataset [(paper_dataset$author=="dispt"),]


fedDataSet_filtered$author <- as.character(fedDataSet_filtered$author)
fedDataSet_filtered$author <- as.factor(fedDataSet_filtered$author)
#str(fedDataSet_filtered)

####### 1. DATA PREPARATION #######
# randomly selected data set to 70 percent to training and 30 percent to test

dataset = sort(sample(nrow(fedDataSet_filtered), nrow(fedDataSet_filtered)* 0.7, replace =FALSE)) # nrow is for the number of rows
View(dataset)
training = fedDataSet_filtered[dataset,] # training data set
testing = fedDataSet_filtered[-dataset,] # testing data set

View(training)
View(testing)
#summary(training)

##### 2. BUILT AND TUNE DECISION TREE MODEL #####

#install.packages("caret", dependencies = TRUE)	
#install.packages('e1071', dependencies=TRUE)
#install.packages("rpart.plot", dependencies = TRUE)

library(caret)
library(rpart)	
library(rpart)
library(rpart.plot)

#default prediction

decision_tree <- train(author ~ . , data = training, metric = "Accuracy", method = "rpart")
decision_tree_prediction <- predict(decision_tree, newdata = testing, na.action = na.omit, type = "prob")
head(decision_tree_prediction, 10)
print(decision_tree_prediction)

# model prediction no 2 

decision_tree_prediction2 <- predict(decision_tree, newdata = testing, type = "raw")
head(decision_tree_prediction2, 10)

# model tuning 

decision_tree_tuning <- train(author ~. , data = training, method = "rpart", metric = "Accuracy", 
tuneLength = 8)
print(decision_tree_tuning$finalModel)
decision_tree_tuningP <- predict(decision_tree_tuning, newdata = testing, type = "raw")
head(decision_tree_tuningP, 12)
print(decision_tree_tuningP)




##### 3.Final Prediction

dt_predict <- predict(decision_tree, newdata = fedDataSet_disputed, na.action = na.omit, type = "prob")
head(dt_predict,12)
