library('RMySQL')
library('rjson')
source('reuse.r')





## GLOBAL VARIABLES
mydb <- connectdb()


## MAIN

## Get initial list of records to work with
records <- dbGetQuery(mydb, 'SELECT idn FROM train')[,1]#[1:100]
rlen <- length(records)


## Calculate features of training set
message('\nCalculating Features for Trainign Set')
X <- data.frame()
pb <- txtProgressBar(style=3)
for(r in records) {
    X <- rbind(X, getFeat('train', r))
    setTxtProgressBar(pb, r/rlen)
}
close(pb)

## Define target and features
target <- c('is_iceberg')
feat <- colnames(X)[!(colnames(X) %in% c(target, 'inc_angle'))]


## Simple feature selection algorithm
message('\nPerforming Feature Selection')

Feat.Selection <- data.frame(feat.order=character(), logloss.train=numeric(), logloss.test=numeric())
feat.cand <- feat                                      # initial list of candidate features

while(TRUE) {
    if(length(feat.cand) == 0) break                   # if no more features, break
    LL <- data.frame(train=numeric(), test=numeric())
    for(f in feat.cand) {                              # try all candidate features
        ## record log loss on test partition
        LL <- rbind(LL, VFXV(X, c(f, as.character(Feat.Selection$feat.order)), target, 10))  
    }
    best <- which(LL[,2] == min(LL[,2]));              # which feature produced best results on test data
    ## Update feature selection results
    Feat.Selection <- rbind(Feat.Selection,
                         data.frame(feat.order=feat.cand[best],
                                    logloss.train=LL$train[best],
                                    logloss.test=LL$test[best])
                         )
    feat.cand <- feat.cand[-c(best)]                   # remove feature from candidate list
}

message('\nFeature Selection Results:')
print(Feat.Selection)

png('feat_sel.png')
op <- par(mar=c(12,4,4,2)+0.1) # default margins
plot(Feat.Selection[,2],type='b',col='blue',main='Feature Selection',
     lty=1, pch=18, axes=FALSE, ann=FALSE,
     ylim=c(min(Feat.Selection$logloss.train),max(Feat.Selection$logloss.test)))
axis(1, at=1:nrow(Feat.Selection), labels=Feat.Selection$feat.order, las=2, cex.axis=1)
axis(2, cex.axis=1)
title(main='Feature Selection', ylab='Log Loss')
title(xlab='Features Added', cex.lab=1, line=10)
lines(Feat.Selection$feat.order, Feat.Selection$logloss.test, type='b', col='red', lty=2, pch=19)
legend('top', legend=c('Training', 'Test'), col=c('blue','red'), lty=1:2)
par(op)
dev.off()


#legend('top', legend=c('Training', 'Test'), col=c('blue','red'), lty=1:2)

## Select those features that decrease the Log Loss of test data
for(i in 2:nrow(Feat.Selection)) {
    if(Feat.Selection$logloss.test[i] > Feat.Selection$logloss.test[i-1]) {
        feat.selected <- as.character(Feat.Selection$feat.order[1:(i-1)])
        break
    }
}

message('\nFeatures Selected:')
print(feat.selected)


## Train Model on entire training set
message('\nTraining Final Model')
model <- glm(formula = is_iceberg ~ ., data = X[,c(feat.selected, target)], family = binomial(logit))


## Classify Unseen Data
message('\nClassifying Unseen Test Data')
out <- data.frame()
records2 <- dbGetQuery(mydb, 'SELECT idn FROM test')[,1]#[1:100]
r2len <- length(records2)
pb <- txtProgressBar(style=3)
for(r in records2) {
    setTxtProgressBar(pb, r/r2len)
    f <- getFeat('test', r)[,feat.selected, drop=FALSE]
    id <- getJAtr('test', 'id', p('idn=', r))
    pr <- suppressWarnings(predict(model, f, type='response'))
    out <- rbind(out, data.frame(id=id, is_iceberg=pr))
}

## Write predictions to file
write.csv(out, file='out.csv', row.names=FALSE)

message('\nComplete!')


disconnectdb(mydb)
