# rm(list = ls(all = TRUE))
# gc()
# gc()

setwd("~/kaggle-criterio/")
options(stringsAsFactors = TRUE)
library(randomForest)
library(gbm)
library(caret)
library(doMC)
print('loading training data')
set.seed(222)
registerDoMC(cores=4)



train.data <- read.csv("train/train.csv", nrows=1000)
test.data <- read.csv("test/test.csv", nrow = 1000)

max_levels =100 
grid <- expand.grid(interaction.depth = c(5),
                    n.trees = c(20),
                    shrinkage = c(0.1))


for(prop in names(train.data[3:15])){
  print(prop)
  train.data[prop] = na.roughfix(train.data[prop])
  test.data[prop] = na.roughfix(test.data[prop])
}


for(prop in names(test.data[15:ncol(test.data)])){
  
  #   prop = 'C6'
  print(prop)
  
  dummy_var = paste(prop,"other")
  
  levels(train.data[,prop]) = c(dummy_var, levels(train.data[,prop]))
  levels(test.data[,prop]) = c(dummy_var, levels(test.data[,prop]))
  
  train.uniques = unique(train.data[,prop])
  
  test.col = test.data[,prop]
  test.idx = test.col %in% train.uniques
  test.idx.nomatch = ifelse(test.idx,FALSE,TRUE)
  test.col[test.idx.nomatch] = dummy_var
  
  test.col = as.character(test.col)
  test.col[which(test.col=="")]=dummy_var
  
  test.col = as.factor(test.col)
  str(test.col)
  test.col[1]= dummy_var
  test.data[,prop]=factor(test.col)
  
  train.data[1,prop] = dummy_var
  
  train.col = train.data[,prop]
  train.col = as.character(train.col)
  train.col[which(train.col=="")]=dummy_var
  train.data[,prop]=factor(train.col)
  
}

 

props = c()

for(prop in names(test.data[15:ncol(test.data)])){
  
  print(prop)
  print(length(levels(test.data[,prop])))
  if(length(levels(test.data[,prop]))<max_levels){
    props= c(props,prop)
  }
  
}

train.data$Label = as.factor(train.data$Label)
train.data$Label = factor(ifelse(train.data$Label==0,'No','Yes'))

formula <- paste(c(names(train.data)[c(3:15)],props)
                 , sep = " ", collapse = " + ")

formula <- paste("Label ~ ", formula, sep = "")
formula




print('train rf reg')

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE)

gbmTune <- train(as.formula(formula),
                 data = train.data,
                 method = "gbm",
                 tuneGrid = grid,
                 verbose=TRUE,
                 trControl = ctrl)

# ggplot(gbmTune) + theme(legend.position = "top")

print('get predictions')

my.pred = predict(gbmTune, newdata=test.data,type='prob')

df.result = data.frame(Id=test.data$Id, Predicted=my.pred$Yes)

head(df.result)

write.csv(file="gbm_caret_trial_v4_100000_train_size_100_max_levels.csv", x=df.result, quote=FALSE, row.names=FALSE)








