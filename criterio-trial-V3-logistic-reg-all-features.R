
rm(list = ls(all = TRUE))
gc()
gc()
setwd("~/kaggle-criterio/")
options(stringsAsFactors = TRUE)
library(randomForest)
print('loading training data')
set.seed(1)

# 45840618/100000
train.data <- read.csv("train/train.csv", nrows=45840618/100)
test.data <- read.csv("test/test.csv", nrow = -1)

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

max_levels=500
props = c()

for(prop in names(test.data[15:ncol(test.data)])){
  
  print(prop)
  print(length(levels(test.data[,prop])))
  if(length(levels(test.data[,prop]))<max_levels){
    props= c(props,prop)
  }
  
}

formula <- paste(c(names(train.data)[c(3:15)],props)
                 , sep = " ", collapse = " + ")
formula <- paste("Label ~ ", formula, sep = "")
formula

#gc()

# print('train log reg')
# my.fit = glm(formula, data = train.data, family= binomial)
# summary(my.fit)
# print('get predictions')
# my.pred = predict(my.fit, type="response", test.data)

print('train rf reg')
my.fit = glm(formula, data = train.data, family= binomial)
summary(my.fit)
print('get predictions')
my.pred = predict(my.fit, type="response", test.data)


df.result = data.frame(Id=test.data$Id, Predicted=glm.pred)

head(df.result)

write.csv(file="glm_v3_features_10000_levels_div_train_sample_100.csv", x=df.result, quote=FALSE, row.names=FALSE)
