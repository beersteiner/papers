library('RMySQL')
library('rjson')
source('reuse.r')





## GLOBAL VARIABLES
mydb <- connectdb()

## Features ranked visually (by histograms)
features <- c('is_iceberg',
              'band.1.max',
              'band.2.max',
              'band.1.tar.mean',
              'band.2.tar.mean',
              'tb.mean.dif.dif',
              'tar.mean.dif',
              'band.1.tb.mean.dif',
              'band.2.tb.mean.dif',
              'band.1.tar.ghs.mean',
              'band.2.tar.ghs.mean',
              'band.1.tar.gvs.mean',
              'band.2.tar.gvs.mean',
              'tar.mean.dif')


## MAIN

## Get initial list of records to work with
records <- dbGetQuery(mydb, 'SELECT idn FROM train')[,1]#[1:100]
rlen <- length(records)


## Calculate features of training set
message('\nCalculating Features for Trainign Set')
X <- data.frame()
pb <- txtProgressBar(style=3)
for(r in records) {
    X <- rbind(X, getFeat('train', r)[,features])
    setTxtProgressBar(pb, r/rlen)
}
close(pb)


## V-Fold Cross Validation
if(TRUE) {
    V <- 10
    fsize <- floor(rlen / V)
    LogLoss = data.frame(train=NA, test=NA)
    ## Loop through each fold
    for(v in 1:V) {
        message(p('\nTraining on fold ', v))
        r.first <- ((v - 1) * fsize) + 1 ## calc. starting index for test
        if(v == V) { ## calc. stopping index for test
            r.last <- rlen ## if last fold
        } else {
            r.last <- v * fsize ## if not last fold
        }
        records.train <- records[-c(r.first:r.last)] ## training set
        records.test <- records[r.first:r.last]      ## testing set
        ## Build training features
        Xr <- X[records.train,]
        model <- glm(formula = is_iceberg ~ .,
                     data = Xr,
                     family = binomial(logit) )
        ## Estimate probabilities & error for training data
        pr.train <- predict(model, Xr, type='response');
        LogLoss[v,'train'] <- logloss(Xr$is_iceberg, pr.train)
        ## Estimate probabilities for test data
        Xs <- X[records.test,]
        pr.test <- predict(model, Xs, type='response')
        LogLoss[v,'test'] <- logloss(Xs$is_iceberg, pr.test)
        ##print(LogLoss[v,])
    }
    message('\nLog Loss per fold:')
    print(LogLoss)
    message('\nAverage Log Loss')
    print(colMeans(LogLoss))
}


## Train Model on entire training set
message('\nTraining Final Model')
model <- glm(formula = is_iceberg ~ ., data = X, family = binomial(logit))


## Classify Unseen Data
message('\nClassifying Unseen Test Data')
out <- data.frame()
records2 <- dbGetQuery(mydb, 'SELECT idn FROM test')[,1]#[1:200]
r2len <- length(records2)
pb <- txtProgressBar(style=3)
for(r in records2) {
    setTxtProgressBar(pb, r/r2len)
    feat <- getFeat('test', r)[,features]
    id <- getJAtr('test', 'id', p('idn=', r))
    pr <- predict(model, feat, type='response')
    out <- rbind(out, data.frame(id=id, is_iceberg=pr))
}

## Write predictions to file
write.csv(out, file='out.csv', row.names=FALSE)

message('\nComplete!')


disconnectdb(mydb)
