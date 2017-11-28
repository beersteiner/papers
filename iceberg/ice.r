library('RMySQL')
library('rjson')


## FUNCTION DEFINITIONS

## This is a function so it can be invoked easily from interactive R session
connectdb <- function() {
    ## Connect to database
    db <- dbConnect(MySQL(), user='john', password='', host='127.0.0.1')
    dbSendQuery(db, 'USE iceberg')
    return(db)
}

## Same here
disconnectdb <- function(db) {
    ## Clean up
    ##dbClearResult(dbListResults(mydb)[[1]])
    dbDisconnect(db)
}


## Wrapper function for paste for compactness
p <- function(...) {
    return(paste(..., sep=''))
}

## This will get a json key value given the table, key name, and condition
getJAtr <- function(tbl, key, cond) {
    qry <- p('SELECT img->"$.',key,'" FROM ', tbl, ' WHERE ', cond)
    suppressWarnings(res <- dbGetQuery(mydb, qry)[,1])
    for(i in 1:length(res)) res[i] <- fromJSON(res[i])
    return(res)
}

## This will return one of the json images in matrix form
getJImg <- function(tbl, bnd, idn) {
    qry <- p('SELECT img->"$.band_', bnd, '" FROM ', tbl, ' WHERE idn=', idn)
    suppressWarnings(tmp <- dbGetQuery(mydb, qry))
    res <- matrix(fromJSON(as.character(tmp)), ncol=75, byrow=TRUE)
    return(res)
}

## This will get the features from idn that are used in the LR
getFeat <- function(tbl, idn) {
    res <-  data.frame(is_iceberg = NA)
    res$b1max <- max(getJImg(tbl, 1, idn))
    res$b2max <- max(getJImg(tbl, 2, idn))
    if(tbl=='train') {
        res$is_iceberg <- as.numeric(getJAtr(tbl, 'is_iceberg', p('idn=', idn)))
    } else {
        res$is_iceberg <- NA
    }
    return(res)
}







## GLOBAL VARIABLES




## MAIN

mydb <- connectdb()


## Get initial list of records to work with
records <- sample(dbGetQuery(mydb, 'SELECT idn FROM train')[,1])
rlen <- length(records)


## ## V-Fold Cross Validation
## V <- 10
## fsize <- floor(rlen / V)
## Error = data.frame(train=NA, train.base=NA, test=NA, test.base=NA)
## ## Loop through each fold
## for(v in 1:V) {
##     message(p('Training on fold ', v))
##     r.first <- ((v - 1) * fsize) + 1 ## calc. starting index for test
##     if(v == V) { ## calc. stopping index for test
##         r.last <- rlen ## if last fold
##     } else {
##         r.last <- v * fsize ## if not last fold
##     }
##     records.train <- records[-c(r.first:r.last)] ## training set
##     records.test <- records[r.first:r.last]      ## testing set
##     ## Build training features
##     X <- data.frame(); for(r in records.train) X <- rbind(X, getFeat('train', r))
##     ## Fit to a Logistic Regression Model
##     model <- glm(formula = is_iceberg ~ .,
##                  data = X,
##                  family = binomial(logit) )
##     ## Estimate probabilities & error for training data
##     pr.train <- predict(model, X, type='response');
##     Error[v,'train'] <- sum(xor(X$is_iceberg, pr.train>0.5)) / length(pr.train)
##     Error[v,'train.base'] <- (function(x) min(x, 1-x)) (sum(X$is_iceberg) / nrow(X))
##     ## Estimate probabilities for test data
##     X <- data.frame(); for(r in records.test) X <- rbind(X, getFeat('train', r))
##     pr.test <- predict(model, X, type='response')
##     Error[v,'test'] <- sum(xor(X$is_iceberg, pr.test>0.5)) / length(pr.test)
##     Error[v,'test.base'] <- (function(x) min(x, 1-x)) (sum(X$is_iceberg) / nrow(X))
##     print(Error[v,])
## }
## print(colMeans(Error))


## Train Model on entire training set
X <- data.frame(); for(r in records) X <- rbind(X, getFeat('train', r))
model <- glm(formula = is_iceberg ~ ., data = X, family = binomial(logit))

## Classify Unseen Data
out <- data.frame()
for(r in dbGetQuery(mydb, 'SELECT idn FROM test')[,1]) {
    feat <- getFeat('test', r)
    id <- getJAtr('test', 'id', p('idn=', r))
    pr <- predict(model, feat, type='response')
    out <- rbind(out, data.frame(id=id, is_iceberg=pr))
}

write.csv(out, file='out.csv', row.names=FALSE)


disconnectdb(mydb)
