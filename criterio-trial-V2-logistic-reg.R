rm(list = ls(all = TRUE))
#setwd("~/kaggle-criterio/")
options(stringsAsFactors = TRUE)

library(randomForest)

print('loading training data')
set.seed(1)

train.data <- read.csv("train/train.csv",nrows=45840618/100)


print('loading test data')
test.data <- read.csv("test/test.csv",nrows=6042136)
test.data = na.roughfix(test.data)

print('fix training data')
train.numeric = train.data[,2:15]
train.numeric$Label = as.factor(train.numeric$Label)
train.numeric=na.roughfix(train.numeric)


#############################
###############fit on all the data
print('train log reg')

glm.fit = glm(Label ~ ., data = train.numeric, family= binomial)

print('get predictions')

glm.pred = predict(glm.fit, type="response", newdata=test.data)

df.result = data.frame(Id=test.data$Id, Predicted=glm.pred)

print('store result')

write.csv(file="glm_v1_div_train_by_100.csv", x=df.result, quote=FALSE, row.names=FALSE)

