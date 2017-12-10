# this file is only used for analyzing the data. All the prediction is done in predict.R with the functions from functions_preprocess.R
rawData <- read.csv("BADS_WS1718_known.csv")
source("functions_preprocess.R")

allData <- preprocess1(rawData)
params <- preprocess2_params(allData)
preprocessedData <- preprocess2(allData, params)

infl <- function(x, y) {
  print(sort(table(x)))
  print(prop.table(table(x, y), 1))
}

infl(allData$order_month, allData$return)
infl(rawData$user_title, rawData$return)

infl(preprocessedData$delivery.time, preprocessedData$return)
infl(allData$delivery.time, allData$return)

sum(preprocessedData$delivery.time>7)

foo<-rawData[rawData$delivery_date=="?",]


sum(foo$return)


infl(preprocessedData$delivery.time, preprocessedData$return)

#chisq test for delivery time
tmp <- preprocessedData[preprocessedData$delivery.time>=0,]
chisq.test(tmp$delivery.time, tmp$return)
cor(as.numeric(tmp$delivery.time), tmp$return)

