library(pROC)
library(mice)
library('RMySQL')
library('grid')
library(rjson)
library(OpenImageR)
library(readr)
library(ROSE)
library(xgboost)



#db connection function
connectdb <- function() {
  ## Connect to database
  db <- dbConnect(MySQL(), user='ashay', password='1234', host='127.0.0.1')
  dbSendQuery(db, 'USE iceberg')
  return(db)
}

disconnectdb <- function(db) {
  suppressWarnings(dbDisconnect(db))
}

#Import Data from DB record by record
## Wrapper function for paste for compactness
p <- function(...) {
  return(paste(..., sep=''))
}

## This will get a json key value given the table, key name, and condition
getJAtr <- function(tbl, key, cond) {
  qry <- p('SELECT img->"$.',key,'" FROM ', tbl, ' WHERE ', cond)
  ## Issues with connection object corruption, so need to catch
  ##   errors and press on
  res <- tryCatch(suppressWarnings(dbGetQuery(mydb, qry)[,1]),
                  error = function(e) {
                    ## warning(call.=FALSE,
                    ##         expr=p('mydb reestablished at getJAtr(',
                    ##                tbl,',',key,',',cond,')'))
                    disconnectdb(mydb)
                    assign('mydb', connectdb(), .GlobalEnv)
                    return(suppressWarnings(dbGetQuery(mydb, qry)[,1]))
                  })
  ## Extract json object from resulting string
  for(i in 1:length(res)) {
    tmp <- fromJSON(res[1]) ## if a vector was returned, it is an img
    if(length(tmp) > 1) return(tmp)
    res[i] <- fromJSON(res[i]) ## otherwise replace result with json object
  }
  return(res)
}


getID = function(idn) {
  res = getJAtr('test', 'id', p('idn=',idn))
  return(res)
}


## This will return one of the json images in matrix form
getJImg = function(tbl, bnd, idn) {
  #print(p(tbl,'/',bnd,'/',idn))
  res <- getJAtr(tbl, p('band_',bnd), p('idn=',idn))
  #print('success')
  return(matrix(res, ncol=75, byrow=TRUE))
}


## This will return inc_angle and handle NAs
getJinc_angle <- function(tbl, idn) {
  return((function(x) if(!(x %in% c('na'))) return(as.numeric(x)) else return(NA))
         (getJAtr(tbl,'inc_angle', p('idn=',idn))))
}


getFeat = function(tbl, idn) {
  res = rep(NA,20)
  
  ## Get image and other items needed to calculate features
  band_1 = getJImg(tbl, 1, idn)
  band_2 = getJImg(tbl, 2, idn)
  inc_angle = getJinc_angle(tbl,idn)
  
  #Calculate HOG feature for both Images
  band1_HOG = as.numeric(HOG(image = matrix(band_1,nrow = 75,ncol = 75),cells = 1,orientations = 9))
  band2_HOG = as.numeric(HOG(image = matrix(band_1,nrow = 75,ncol = 75),cells = 1,orientations = 9))
  
  res[1:9] = band1_HOG
  res[10:18] = band2_HOG
  res[19] = inc_angle
  
  ## only extract label if this is training data
  if(tbl == 'train') {
    res[20] = as.numeric(getJAtr(tbl, 'is_iceberg', p('idn=', idn)))
  } 
  ## Return features
  return(res)
}





#Training Dataset is imported row by row. band_1 and band_2 images are processed to get HOG Descriptors out of it. The train_data matrix is populated with Processed Training Dataset 

mydb = connectdb()
records = dbGetQuery(mydb, 'SELECT idn FROM train')[,1]#[1:100]
rlen = length(records)

write('\nImporting Training Set and calculating HOG Descriptors', stdout())
train_data = matrix(nrow = rlen,ncol = 20)
colnames(train_data) = c('B1_1','B1_2','B1_3','B1_4','B1_5','B1_6','B1_7','B1_8','B1_9','B2_1','B2_2','B2_3','B2_4','B2_5','B2_6','B2_7','B2_8','B2_9','inc_angle','is_iceburg')
pb <- txtProgressBar(style=3, file=stderr())
for(r in records) {
  train_data[r,] = getFeat('train', r)
  setTxtProgressBar(pb, r/rlen)
}
close(pb)


#Testing Dataset is imported row by row. band_1 and band_2 images are processed to get HOG Descriptors out of it. The train_data matrix is populated with Processed Training Dataset
records = dbGetQuery(mydb, 'SELECT idn FROM test')[,1]
rlen = length(records)

write('\nImporting Testing Set and calculating HOG Descriptors', stdout())
test_data = matrix(nrow = rlen,ncol = 19)
test_id = rep(NA,rlen)
colnames(test_data) = c('B1_1','B1_2','B1_3','B1_4','B1_5','B1_6','B1_7','B1_8','B1_9','B2_1','B2_2','B2_3','B2_4','B2_5','B2_6','B2_7','B2_8','B2_9','inc_angle')
pb <- txtProgressBar(style=3, file=stderr())
for(r in records) {
  test_data[r,] = getFeat('test', r)[1:19]
  test_id[r] = getID(r)
  setTxtProgressBar(pb, r/rlen)
}
close(pb)


#Checking for Null Values
colSums(is.na(train_data))
#Training Data has 133 Null Values in inc_angle feature

colSums(is.na(test_data))
#Test Data does not have any Null Values


#Let's try to impute missing values using mice package 

#It gives Pattern of missing values
##md.pattern(train_data)

#mice function will predict missing values based on parameters
#m=5 this will impute 5 different dataset
#maxit = 20 maximum iteration taken to impute missing value
#method = 'pmm' Predictive mean matching as the inc_angle is continuous data
imputed_Data = mice(train_data, m=5, maxit = 20, method = 'pmm', seed = 500)

training_data = complete(imputed_Data,1)[1:1000,1:19]
training_label = complete(imputed_Data,1)[1:1000,20]
validation_data = complete(imputed_Data,1)[1001:1604,1:19]
validation_label = complete(imputed_Data,1)[1001:1604,20]


#Actual Loop

models = list()
accuracy = rep(0,5)
auc = rep(0,5)
loss = rep(0,5)
for (i in 1:5) {
  training_data = complete(imputed_Data,i)[1:1000,1:19]
  training_label = complete(imputed_Data,i)[1:1000,20]
  validation_data = complete(imputed_Data,i)[1001:1604,1:19]
  validation_label = complete(imputed_Data,i)[1001:1604,20]
  
  model = xgboost(data = as.matrix(training_data),label = training_label, nrounds = 100, max.depth = 2,eta = 1, nthread = 2, objective = "binary:logistic",save_period = NULL)
  models[[i]] = model
  
  pred_xgb = predict(model,as.matrix(validation_data))
  
  prediction = rep(0,length(pred))
  prediction[pred_xgb > 0.5] = 1
  
  acc = mean(prediction == validation_label) * 100
  accuracy[i] = acc
  cat("Model ",i," - Accuracy - ",acc,"\n")
  
  roc.curve(validation_label,pred_xgb)
  
  auc[i] = auc(validation_label,pred_xgb)
  
  loss[i] = logloss(validation_label,pred_xgb)
  
}

model = models[[which.max(auc)]]
log_loss = loss[which.min(loss)]

#0.6610249

auc_val = auc[which.max(auc)]
auc_val
#0.8170044
which.max(auc)

test_prediction = predict(model,as.matrix(test_data))

result = cbind(test_id,test_prediction)
colnames(result) = c("id","is_iceberg")
result = as.data.frame(result)
write_csv(x = result,path =  paste("prediction.csv",sep = ""),append = FALSE,col_names = TRUE)

# Kaggle score 0.5986



#Extracting important features and performing logistic on it
importace_matrix = xgb.importance(colnames(train_data),model)

xgb.plot.importance(importace_matrix)

imp_feature = importace_matrix$Feature

training_data = complete(imputed_Data,5)[1:1000,imp_feature]
training_label = complete(imputed_Data,5)[1:1000,20]
validation_data = complete(imputed_Data,5)[1001:1604,imp_feature]
validation_label = complete(imputed_Data,5)[1001:1604,20]

training_data = cbind(training_data,training_label)

model = glm(training_label ~ ., data = as.data.frame(training_data), family = binomial(link = 'logit'))

pred_logit = predict(model,validation_data,type = 'response')
prediction = rep(0,length(pred_logit))
prediction[pred_logit > 0.5] = 1

acc = mean(prediction == validation_label) * 100
acc

roc.curve(validation_label,pred_logit)

auc(validation_label,pred_logit)
#0.7129

logloss(validation_label,pred_logit)
#0.6266353

