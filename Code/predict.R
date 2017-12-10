#setwd("~/BADS_WS1718_known.csv")
if (!require("caret"))
  install.packages("caret")
library("caret")
if (!require("e1071"))
  install.packages("e1071")
library("e1071")
library(rpart)
if (!require("randomForest"))
  install.packages("randomForest")
library("randomForest")
if (!require("hmeasure"))
  install.packages("hmeasure")
library("hmeasure")

source("functions_preprocess.R")

rawData <- read.csv("BADS_WS1718_known.csv")

allData <- preprocess1(rawData)

# split data
ratio = 0.8
#set.seed(123)
p = dim(allData)[1] * ratio
s <- sample(dim(allData)[1])[1:p]
dataTrain <- allData[s, ]
dataTest <- allData[-s, ]

params <- preprocess2_params(dataTrain)
preprocessedDataTrain <- preprocess2(dataTrain, params)
preprocessedDataTest <- preprocess2(dataTest, params)
prediction <- list()
reality <- preprocessedDataTest$return

# try decision trees

fitDT <-
  rpart(
    return ~ item_price + delivery.time_factor + legal.age + user_state + user_title,
    data = preprocessedDataTrain,
    method = "class"
  )

prediction[["dt"]] <-
  predict(fitDT, preprocessedDataTest, type = "class")

correctPerc <- sum(prediction[["dt"]] == reality) / dim(preprocessedDataTest)[1]
print("======================= decision tree")
print(correctPerc)
print(confusionMatrix(prediction[["dt"]], reality, positive = "1"))

# try random forest

fitRF <-
  randomForest(
    as.factor(return) ~ item_price + delivery.time_factor + legal.age + brand_id + user_reg_date + order_month,
    data = preprocessedDataTrain,
    importance = TRUE,
    ntree = 500,
    maxnodes = 20
  )

prediction[["rf"]] <- predict(fitRF, preprocessedDataTest)

correctPerc <-
  sum(prediction[["rf"]] == reality) / dim(preprocessedDataTest)[1]

print("======================= random forest")
print(correctPerc)
print(confusionMatrix(prediction[["rf"]], reality, positive = "1"))


#Logistic regression
#Logistic regression is one of the standard models in predictive analytics, modeling the outcome of a categoric variable by a linear combination of independent variables transformed in a way to predict outcome probabilities between 0 and 1.

lrFun <- return ~ delivery.time_factor + item_id + item_color + brand_id + item_price + user_id + user_title + user_state + user_reg_date + legal.age
lr <-
  glm(
    lrFun,
    data = preprocessedDataTrain,
    family = binomial(link = 'logit')
  )

prediction[["lr"]] <-
  predict(lr, preprocessedDataTest, type = "response")
correctPerc <- 0
# for (t in seq(from = 0, to = 1, by = 0.1)) {
#   ptmp <- as.numeric(prediction[["lr"]] > t)
#   correctPerc <-
#     sum(ptmp == reality) / dim(preprocessedDataTest)[1]
#   print(correctPerc)
#   #print(confusionMatrix(prediction, reality, positive = "1"))
# }
ptmp <- as.numeric(prediction[["lr"]] > 0.5)
correctPerc <-
  sum(ptmp == reality) / dim(preprocessedDataTest)[1]


print("======================= logistic regression")
print(correctPerc)
print(confusionMatrix(ptmp, reality, positive = "1"))


# stuff with roc


yhat.df <- data.frame(prediction)  
h <- HMeasure( reality, yhat.df, severity.ratio = 0.1) 
auc_testset <- h$metrics['AUC']
auc_testset

### predict the unknown data, this time train on all known data
rawUnknownData <- read.csv("BADS_WS1718_class.csv")
#preprocess
allUnknownData <- preprocess1(rawUnknownData)
paramsAll <- preprocess2_params(allData)
preprocessedDataTrainAll <- preprocess2(allData, paramsAll)
preprocessedUnknownData <- preprocess2(allUnknownData, paramsAll)

#train lr model with all data
lrAll <-
  glm(
    lrFun,
    data = preprocessedDataTrainAll,
    family = binomial(link = 'logit')
  )

#predict using logistic regression, as sensitivity and specificity are more balanced, even though overall accuracy is slightly lower than DT and RF
df <-
  data.frame(rawUnknownData$order_item_id, as.numeric(predict(lrAll, preprocessedUnknownData,  type = "response") > 0.5))
names(df) <- c("order_item_id", "return")
#save
write.csv(df, "594644_aydin.csv", row.names = FALSE)


