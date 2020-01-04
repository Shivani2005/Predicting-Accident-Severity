######  Installing the required packages
install.packages('mice')
install.packages('caret')
install.packages('randomForest')

#####  Including the required libraries
library('mice')
library('class')
library('caret')
library('e0171')
library('randomForest')

### Loading & understanding the dataset
data <- read.csv('C:/Users/17815/Downloads/dftRoadSafetyData_Accidents_2018.csv')
head(data)
names(data)
summary(data)
dim(data)
View(data)

#### Correlation Matrix corresponding all the variables
corMatrix <- cor(data[, -c(1, 10, 12, 14, 32)])
corMatrix

####  Filtering the variables
data1 <- data[,c("Longitude", "Latitude", "Accident_Severity", "Number_of_Casualties", "Date", "Time", "Road_Type", "Speed_limit", 
                 "Junction_Detail", "Junction_Control", "Light_Conditions", "Weather_Conditions", "Road_Surface_Conditions", 
                 "Special_Conditions_at_Site","Carriageway_Hazards", "Urban_or_Rural_Area", "Did_Police_Officer_Attend_Scene_of_Accident")]

####  Removing the records with NAs
colSums(is.na(data1))
data1 <- na.omit(data1)

#### COnfirming correlation among the filtered variables
cormatrix_1 <- cor(data1[, -c(5, 6)])
cormatrix_1
n <- nrow(data1)

sapply(data1, class)
### Converting numerical data to categorical where ever necessary
data1$Road_Type <- as.factor(data1$Road_Type)
data1$Junction_Control <- as.factor(data1$Junction_Control)
data1$Road_Surface_Conditions <- as.factor(data1$Road_Surface_Conditions)
data1$Urban_or_Rural_Area <- as.factor(data1$Urban_or_Rural_Area)
data1$Light_Conditions <- as.factor(data1$Light_Conditions)
data1$Special_Conditions_at_Site <- as.factor(data1$Special_Conditions_at_Site)
data1$Did_Police_Officer_Attend_Scene_of_Accident <- as.factor(data1$Did_Police_Officer_Attend_Scene_of_Accident)
data1$Accident_Severity <- as.factor(data1$Accident_Severity)
data1$Junction_Detail <- as.factor(data1$Junction_Detail)
data1$Weather_Conditions <- as.factor(data1$Weather_Conditions)
data1$Carriageway_Hazards <- as.factor(data1$Carriageway_Hazards)

### Checking the class of each varibale 
sapply(data1, class)
summary(data1)
colSums(is.na(data1))


############  MODELLING  ################
sample <- sample(1:n, n*0.8)
train <- data1[sample, ]
test <- data1[-sample, ]
train_target <- train$Accident_Severity
test_target <- test$Accident_Severity
train <- train[,-c(5, 6)]
summary(train)
test <- test[, -c(5, 6)]
dim(train)
dim(test)

####  Random Forest Classifier ####

rf <- randomForest(
  Accident_Severity ~ ., ntree = 500,
  data=train
)

#####  Predicting  #####
pred = predict(rf, newdata=test[-3])

####  Creating a confusion matrix
confusionMatrix(pred, test_target)




