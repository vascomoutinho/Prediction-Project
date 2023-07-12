setwd("C:/Users/sbran/OneDrive/Ambiente de Trabalho")
db <- read.csv("predictive_maintenance.csv")

###################################### [Data preparation] ###################################

str(db)
db[,c(1:3,9,10)] <- lapply(db[,c(1,2,3,9,10)], as.factor)
db[,-c(1:3,9,10)] <- lapply(db[,-c(1,2,3,9,10)], as.numeric)

#Missing values:
summary(db) 
sum(is.na(db)) #db doesn't have missing values

#Realize if all target=0 correspond to 0 failures
db$Target[db$Target == 0]
db$Failure.Type[db$Failure.Type == "No Failure"]

db$Target[db$Failure.Type == "Random Failures"] <-"1"
db$Failure.Type[db$Target == 0] <- "No Failure"
db$Target[db$Failure.Type == "No Failure"] <- "0"

summary(db$Failure.Type)

####################################[Descriptive Analysis]###################################
summary(db)

#Mean and standard deviation 
mean <- unlist(lapply(db[,-c(1:3,9,10)], mean))
sd <- unlist(lapply(db[,-c(1:3,9,10)], sd))
median<-unlist(lapply(db[,-c(1:3,9,10)], median))
mean
sd
median

#minimums and maximums
max <- unlist(lapply(db[,-c(1:3,9,10)], max))
min <- unlist(lapply(db[,-c(1:3,9,10)], min))
max
min

###############################################################################
#-------------------------Types of failures by quality----------------------#
pairs(db[, c(4:8)])

#Types of failures with high-quality machines
library(ggplot2)
high_quality_failures <- subset(db, grepl("^H", Product.ID) & Target == 1)

ggplot(high_quality_failures, aes(x = Failure.Type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Failure Type for High Quality Products", 
       x = "Failure Type", y = "Count")

#Types of failures with medium quality machines 
medium_quality_failures <- subset(db, grepl("^M", Product.ID) & Target == 1)

ggplot(medium_quality_failures, aes(x = Failure.Type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Failure Type for Medium Quality Products ", 
       x = "Failure Type", y = "Count")

#Types of failures with low quality machines
low_quality_failures <- subset(db, grepl("^L", Product.ID) & Target == 1)

ggplot(low_quality_failures, aes(x = Failure.Type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Failure Type for Low Quality Products", 
       x = "Failure Type", y = "Count")

###############################################################################
#-----------------------Understand the reasons for failures-------------------#

#Failures varying with air temperature
ggplot(db[db$Target == 1, ], aes(x = Air.temperature..K., fill = Failure.Type)) + 
  geom_density(alpha = 0.6) + 
  labs(x = "Air Temperature (K)", y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Density plot of Air Temperature by Type of Failure") 


#Failures varying with processing temperature
ggplot(db[db$Target == 1, ], aes(x = Process.temperature..K., fill = Failure.Type)) + 
  geom_density(alpha = 0.6) + 
  labs(x = "Process Temperature (K)", y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Density plot of Process Temperature by Type of Failure") 

#Failures varying with Rotation speed
ggplot(db[db$Target == 1, ], aes(x = Rotational.speed..rpm., fill = Failure.Type)) + 
  geom_density(alpha = 0.6) + 
  labs(x = "Rotational Speed (rpm)", y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Density plot of Rotational Speed by Type of Failure") 

#Failures varying with torque
ggplot(db[db$Target == 1, ], aes(x = Torque..Nm., fill = Failure.Type)) + 
  geom_density(alpha = 0.6) + 
  labs(x = "Torque (Nm)", y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Density plot of Torque by Type of Failure") 

#Failures varying with time of use
ggplot(db[db$Target == 1, ], aes(x = Tool.wear..min., fill = Failure.Type)) + 
  geom_density(alpha = 0.6) + 
  labs(x = "Tool Wear (min)", y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Density plot of Tool Wear by Type of Failure") 

#Correlations
cor(db[,c(4:8)])
library(corrplot)
corrplot(cor(db[,c(4:8)]),order="AOE")
library(GGally)
GGally::ggpairs(db[,c(4:8)])

#Outliers
boxplot(db$Air.temperature..K.,main="BoxPlot Air Temperature")
boxplot(db$Process.temperature..K., main="BoxPlot Process Temperature")
boxplot(db$Rotational.speed..rpm., main="BoxPlot Rotational Speed")
boxplot(db$Torque..Nm., main="BoxPlot Torque")
boxplot(db$Tool.wear..min., main="BoxPlot Tool Wear")

outliers<-boxplot.stats(db$Rotational.speed..rpm.)$out
outliers1<-boxplot.stats(db$Torque..Nm.)$out

#Plot Outliers
plot(x = db$Rotational.speed..rpm., y = db$Torque..Nm., 
     main = "Scatterplot of Rotational Speed and Torque", 
     xlab = "Rotational Speed (rpm)", ylab = "Torque (Nm)")
points(x = outliers, y = rep(max(db$Torque..Nm.), length(outliers)), 
       col = "red", pch = 19)
points(x = rep(max(db$Rotational.speed..rpm.), length(outliers1)), y = outliers1, 
       col = "blue", pch = 19)


##################################### [Predictive Analysis] ####################################

#Split the data
#install.packages("splitTools")
library(splitTools)
set.seed(7)
part_T <- partition(db$Target, c(train=0.7, test=0.3))
part_FT <- partition(db$Failure.Type, c(train=0.7, test=0.3))

trainset <- db[part_T$train,]
testset <- db[part_T$test,]
trainset_FT <- db[part_FT$train,]
testset_FT <- db[part_FT$test,]

#Balance the trainset
library(caret)
balanced_train_T <- downSample(trainset,trainset$Target)
balanced_train_FT <- downSample(trainset_FT,trainset_FT$Failure.Type)

##################################### [1st Target] ####################################
###################################### [K-NN] ###################################

#To change the categorical variables in dummy variables (that will be numeric):
#install.packages("fastDummies")
library("fastDummies")
train_dummies <- dummy_cols(balanced_train_T, select_columns=c("Type"), 
                            remove_first_dummy = TRUE,
                            remove_selected_columns = TRUE)
test_dummies <- dummy_cols(testset, select_columns=c("Type"), 
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)
str(train_dummies)
str(test_dummies)
train_dummies[,c(11,12)] <- lapply(train_dummies[,c(11,12)], as.numeric)
test_dummies[,c(10,11)] <- lapply(test_dummies[,c(10,11)], as.numeric)

#install.packages("FNN")
library(FNN)
prediction <- knn(train_dummies[,-c(1,2,8,9,10)], test_dummies[,-c(1,2,8,9)], 
                  train_dummies$Class,k=2) 

#Confusion Matrix
library(caret)

ConfuMatrix <- confusionMatrix(prediction,testset$Target)
ConfuMatrix
ConfuMatrix$overall
ConfuMatrix$byClass

###################################### [Naive Bayes] ###################################

#install.packages("e1071")
library(e1071)
model_NB <- naiveBayes(Class~.-UDI -Product.ID -Failure.Type -Target, balanced_train_T)
str(model_NB)
prediction_NB <- predict(model_NB, testset)

#Confusion Matrix
library(caret)

ConfuMatrix_NB <- confusionMatrix(prediction_NB,testset$Target)
ConfuMatrix_NB
ConfuMatrix_NB$overall
ConfuMatrix_NB$byClass


###################################### [Decision Tree] ###################################

#Information Gain#
library(rpart.plot)
library(rpart)
library(rattle)
mytree <- rpart(Class~.-UDI -Product.ID -Failure.Type -Target, 
               data = balanced_train_T, method = "class", 
               parms = list(split = "information"), minsplit = 4 , minbucket = 3)
fancyRpartPlot(mytree, caption = NULL, cex=0.5)
prediction_tree <- predict(mytree,testset,type="class")

#Confusion Matrix
library(caret) 

ConfuMatrix_tree <- confusionMatrix(prediction_tree,testset$Target)
ConfuMatrix_tree
ConfuMatrix_tree$overall
ConfuMatrix_tree$byClass

#With the tree pruned:
mytree_pruned <- prune(mytree, cp=0.05)
rpart.plot(mytree_pruned, main = "Árvore de Decisão: Target", col = "black", cex = 0.6 , extra=100)
fancyRpartPlot(mytree_pruned, caption = NULL,cex = 0.6)
prediction_tree_pruned <- predict(mytree_pruned,testset,type="class")

#Confusion Matrix
library(caret) 

ConfuMatrix_tree_pruned <- confusionMatrix(prediction_tree_pruned,testset$Target)
ConfuMatrix_tree_pruned
ConfuMatrix_tree_pruned$overall
ConfuMatrix_tree_pruned$byClass


#Gini Index#
mytree_gini <- rpart(Class~.-UDI -Product.ID -Failure.Type -Target, 
               data = balanced_train_T, method = "class", 
               parms = list(split = "gini"), minsplit = 4 , minbucket = 3)
fancyRpartPlot(mytree_gini, caption = NULL, cex=0.5)
prediction_tree_gini <- predict(mytree_gini,testset,type="class")

#Confusion Matrix
library(caret) 

ConfuMatrix_tree_gini <- confusionMatrix(prediction_tree_gini,testset$Target)
ConfuMatrix_tree_gini
ConfuMatrix_tree_gini$overall
ConfuMatrix_tree_gini$byClass

#With the tree pruned:
mytree_pruned_gini <- prune(mytree_gini, cp=0.05)
fancyRpartPlot(mytree_pruned_gini, caption = NULL, cex=0.5)
prediction_tree_pruned_gini <- predict(mytree_pruned_gini,testset,type="class")

#Confusion Matrix
library(caret) 

ConfuMatrix_tree_pruned_gini <- confusionMatrix(prediction_tree_pruned_gini,testset$Target)
ConfuMatrix_tree_pruned_gini
ConfuMatrix_tree_pruned_gini$overall
ConfuMatrix_tree_pruned_gini$byClass

###################################### [Random Forest] ###################################
library(randomForest)
library(ISLR)

set.seed(7)
rndom <- randomForest(Class~ .-UDI -Product.ID -Target -Failure.Type, data = balanced_train_T, importance = TRUE)
plot(rndom)
PredictionR<-predict(rndom, testset, type="class")

#Confusion Matrix
library(caret) 

ConfuMatrix_R <- confusionMatrix(PredictionR,testset$Target)
ConfuMatrix_R
ConfuMatrix_R$overall
ConfuMatrix_R$byClass

################################### [2nd Failure.Type] ####################################
###################################### [K-NN] #########################################

#To change the categorical variables in dummy variables (that will be numeric):
#install.packages("fastDummies")
library("fastDummies")
train_dummies_2 <- dummy_cols(balanced_train_FT, select_columns=c("Type"), 
                            remove_first_dummy = TRUE,
                            remove_selected_columns = TRUE)
test_dummies_2 <- dummy_cols(testset_FT, select_columns=c("Type"), 
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)
str(train_dummies_2)
str(test_dummies_2)
train_dummies_2[,c(11,12)] <- lapply(train_dummies_2[,c(11,12)], as.numeric)
test_dummies_2[,c(10,11)] <- lapply(test_dummies_2[,c(10,11)], as.numeric)

#install.packages("FNN")
library(FNN)
prediction_2 <- knn(train_dummies_2[,-c(1,2,8,9,10)], test_dummies_2[,-c(1,2,8,9)], 
                  train_dummies_2$Class,k=2) 

#Confusion Matrix
library(caret)
library(ISLR)

ConfuMatrix_2 <- confusionMatrix(prediction_2,testset_FT$Failure.Type)
ConfuMatrix_2
ConfuMatrix_2$overall
ConfuMatrix_2$byClass

###################################### [Naive Bayes] ###################################

#install.packages("e1071")
library(e1071)
model_NB_2 <- naiveBayes(Class~.-UDI -Product.ID -Target -Failure.Type, balanced_train_FT)
str(model_NB_2)
prediction_NB_2 <- predict(model_NB_2, testset_FT)

#Confusion Matrix
library(caret)
library(ISLR)

ConfuMatrix_NB_2 <- confusionMatrix(prediction_NB_2,testset_FT$Failure.Type)
ConfuMatrix_NB_2
ConfuMatrix_NB_2$overall
ConfuMatrix_NB_2$byClass

###################################### [Decision Tree] ###################################

#Information Gain#
library(rpart.plot)
library(rpart)
library(rattle)
mytree_2 <- rpart(Class~.-UDI -Product.ID -Target -Failure.Type, data = balanced_train_FT, 
                 method = "class", parms = list(split = "information"), minsplit =4, 
                 minbucket = 3)
fancyRpartPlot(mytree_2, caption = NULL)
prediction_tree_2 <- predict(mytree_2,testset_FT,type="class")

#Confusion Matrix
library(caret)
library(ISLR)

ConfuMatrix_tree_2 <- confusionMatrix(prediction_tree_2,testset_FT$Failure.Type)
ConfuMatrix_tree_2
ConfuMatrix_tree_2$overall
ConfuMatrix_tree_2$byClass

#With the tree pruned:
mytree_pruned_2 <- prune(mytree_2, cp=0.05)
fancyRpartPlot(mytree_pruned_2, caption = NULL)
prediction_tree_pruned_2 <- predict(mytree_pruned_2,testset_FT,type="class")

#Confusion Matrix
library(caret)
library(ISLR)

ConfuMatrix_tree_pruned_2 <- confusionMatrix(prediction_tree_pruned_2,testset_FT$Failure.Type)
ConfuMatrix_tree_pruned_2
ConfuMatrix_tree_pruned_2$overall
ConfuMatrix_tree_pruned_2$byClass

#Gini Index#
mytree_gini_2 <- rpart(Class~.-UDI -Product.ID -Target -Failure.Type, 
                    data = balanced_train_FT, method = "class", 
                    parms = list(split = "gini"), minsplit = 4 , minbucket = 3)
fancyRpartPlot(mytree_gini_2, caption = NULL)
prediction_tree_gini_2 <- predict(mytree_gini_2,testset_FT,type="class")

#Confusion Matrix
library(caret)
library(ISLR)

ConfuMatrix_tree_gini_2 <- confusionMatrix(prediction_tree_gini_2,testset_FT$Failure.Type)
ConfuMatrix_tree_gini_2
ConfuMatrix_tree_gini_2$overall
ConfuMatrix_tree_gini_2$byClass

#With the tree pruned:
mytree_pruned_gini_2 <- prune(mytree_gini_2, cp=0.05)
fancyRpartPlot(mytree_pruned_gini_2, caption = NULL)
prediction_tree_pruned_gini_2 <- predict(mytree_pruned_gini_2,testset_FT,type="class")

#Confusion Matrix
library(caret)

ConfuMatrix_tree_pruned_gini_2 <- confusionMatrix(prediction_tree_pruned_gini_2,testset_FT$Failure.Type)
ConfuMatrix_tree_pruned_gini_2
ConfuMatrix_tree_pruned_gini_2$overall
ConfuMatrix_tree_pruned_gini_2$byClass

###################################### [Random Forest] ###################################
library(randomForest)
library(ISLR)

set.seed(7)
rndom_FT <- randomForest(Class~ .-UDI -Product.ID -Target -Failure.Type, data = balanced_train_FT, importance = TRUE)
plot(rndom_FT)
PredictionR_FT<-predict(rndom_FT, testset_FT, type="class")

#Confusion Matrix
library(caret) 

ConfuMatrix_R_FT <- confusionMatrix(PredictionR_FT,testset_FT$Failure.Type)
ConfuMatrix_R_FT
ConfuMatrix_R_FT$overall
ConfuMatrix_R_FT$byClass
