
#Load Data
data = read.table(file.choose(),header=T,sep=",")

#Summary of possible predictors
summary(data$Sex)
summary(data$Age)

#relationship between sex and survival 
table(data$Sex,data$Survived)
prop.table(table(data$Sex,data$Survived))

#Age as a predictor
data$Child = 0  #creating a new column
data$Child[data$Age<18] = 1
table(data$Child,data$Survived)
prop.table(table(data$Child,data$Survived))
aggregrate(Survived~Child+Sex.............)

#KNN to classify data based on PassengerClass, No of Siblings, no of parents or childerner on board and Fare
library(caret)
library(class)
InTrain = createDataPartition(y=data$Survived, p=0.7, list=FALSE)
train = data[InTrain,]
test = data[-InTrain,]
train_X = train[,c("Pclass","SibSp","Parch","Fare")]
test_X = train[,c("Pclass","SibSp","Parch","Fare")]
test_y = tes
train_y = train[,c("Survived")]
knn_pred=knn(train=train_X, test=test_X, cl= train_y, k=3)
knn_pred_fit=knn_predict.fit(3)
knn_pred_fit_pred = knn_pred_fit.predict()

###########################################################################
#Decision Tree to classify data with interpretability
# import libraries
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#data fit and prediction
fit = rpart(Survived~Sex,train,method="class")
fancyRpartPlot(fit)
test_X = test[,c("Pclass","SibSp","Parch","Fare","Sex")]
dt_predict=predict(fit,test_X,type="class")

#############################################################################
#with multiple features
fit2=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method="class")
fancyRpartPlot(fit2)