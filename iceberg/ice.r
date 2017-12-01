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


## V-Fold Cross Validation
if(TRUE) {
    V <- 10
    fsize <- floor(rlen / V)
    LogLoss = data.frame(train=NA, test=NA)
    ## Loop through each fold
    for(v in 1:V) {
        message(p('Training on fold ', v))
        r.first <- ((v - 1) * fsize) + 1 ## calc. starting index for test
        if(v == V) { ## calc. stopping index for test
            r.last <- rlen ## if last fold
        } else {
            r.last <- v * fsize ## if not last fold
        }
        records.train <- records[-c(r.first:r.last)] ## training set
        records.test <- records[r.first:r.last]      ## testing set
        ## Build training features
        Xr <- data.frame()
        pb <- txtProgressBar(style=3); prog=0
        for(r in records.train) {
            Xr <- rbind(Xr, getFeat('train', r)[,features])
            prog=prog+1
            setTxtProgressBar(pb, prog/length(records.train))
        }
        ## Fit to a Logistic Regression Model
        model <- glm(formula = is_iceberg ~ .,
                     data = Xr,
                     family = binomial(logit) )
        ## Estimate probabilities & error for training data
        pr.train <- predict(model, Xr, type='response');
        LogLoss[v,'train'] <- logloss(Xr$is_iceberg, pr.train)
        ## Error[v,'train'] <- sum(xor(Xr$is_iceberg, pr.train>0.5)) / length(pr.train)
        ## Error[v,'train.base'] <- (function(x) min(x, 1-x)) (sum(Xr$is_iceberg) / nrow(Xr))
        ## Estimate probabilities for test data
        Xs <- data.frame(); for(r in records.test) Xs <- rbind(Xs, getFeat('train', r)[,features])
        pr.test <- predict(model, Xs, type='response')
        LogLoss[v,'test'] <- logloss(Xs$is_iceberg, pr.test)
        ## Error[v,'test'] <- sum(xor(Xs$is_iceberg, pr.test>0.5)) / length(pr.test)
        ## Error[v,'test.base'] <- (function(x) min(x, 1-x)) (sum(Xs$is_iceberg) / nrow(Xs))
        print(LogLoss[v,])
    }
    print(colMeans(LogLoss))
}


## Train Model on entire training set
message('\nTraining Final Model')
Xrt <- data.frame()
pb <- txtProgressBar(style=3)
for(r in records) {
    setTxtProgressBar(pb, r/rlen)
    Xrt <- rbind(Xrt, getFeat('train', r)[,features])
}
model <- glm(formula = is_iceberg ~ ., data = Xrt, family = binomial(logit))

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
