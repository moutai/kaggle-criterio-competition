rm(list = ls(all = TRUE)) 
setwd("~/kaggle-criterio/")
options(stringsAsFactors = TRUE)

# library(gbm)
# library(tree)
library (randomForest)
print('loading training data')
set.seed(1)
#train.data <- read.csv("train/train.csv",nrows=1000)
#train.data <- read.csv("train/train.csv",nrows=1000)

train.data <- read.csv("train/train.csv",nrows=45840618/10)


print('loading test data')
test.data <- read.csv("test/test.csv",nrows=6042136)
#test.data <- read.csv("test/test.csv",nrows=604)
test.data = na.roughfix(test.data)

print('fix training data')
train.numeric = train.data[,2:15]
train.numeric$Label = as.factor(train.numeric$Label)
train.numeric=na.roughfix(train.numeric)


#############################
###############fit on all the data
print('train random forest')
rf.fit = randomForest(Label ~ ., data = train.numeric,
                      mtry=6, importance =TRUE)

print('get predictions')
rf.pred = predict(rf.fit, type="prob", newdata=test.data)[,2]

df.result = data.frame(Id=test.data$Id, Predicted=rf.pred)
print('store result')
write.csv(file="rf_v3_div_train_by_10.csv", x=df.result, quote=FALSE, row.names=FALSE)








