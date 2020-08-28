suppressMessages(library('RMySQL'))
suppressMessages(library('rjson'))
suppressMessages(library('grid'))
suppressMessages(library('pROC'))

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
    suppressWarnings(dbDisconnect(db))
}


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

## This will return one of the json images in matrix form
getJImg <- function(tbl, bnd, idn) {
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
    

## Apply convolution to an image given a 3x3 kernel k
kern <- function(img, k) {
    res <- img
    img <- cbind(img[,1], img, img[,ncol(img)])
    img <- rbind(img[1,], img, img[nrow(img),])
    for(i in 1:nrow(res)) {
        for(j in 1:ncol(res)) {
            res[i,j] = sum(k * img[i:(i+2),j:(j+2)])
        }
    }
    return(res)
}

## Return  mini-matrix around zoom point
zoom <- function(img, pos, rad) {
    rrng <- max(pos[[1]][1]-rad, 1):min(pos[[1]][1]+rad, 75)
    crng <- max(pos[[1]][2]-rad, 1):min(pos[[1]][2]+rad, 75)
    return(img[rrng,crng])
}

## Find the center of object
findCenter <- function(img) {
    res <- list(c(NA,NA))
    rad <- 3
    mx <- -Inf
    for(r in (1+rad):(75-rad)) {
        for(c in (1+rad):(75-rad)) {
            tmp <- sum(zoom(img, list(c(r,c)), rad))
            if(tmp > mx) {
                mx <- tmp
                res <- list(c(r,c))
            }
        }
    }
    return(res)
}

## Returns pixel mask representing pixels for which the smoothed
##  image is greater than some threshold
stripBackground <- function(img) {
    tmp <- kern(img, k.smooth)
    threshold <- 0.7
    return(((tmp - min(tmp)) / (max(tmp) - min(tmp))) > threshold)
}

## Convert any numeric matrix to a raster object that can be plotted
raster <- function(img) {
    res <- (img - min(img)) * (1 / (max(img) - min(img)))
    return(as.raster(res))
}

## Plot color image given 3 input matrices
colorize <- function(img1, img2, img3) {
    r <- (img1 - min(img1)) * (1 / (max(img1)-min(img1)))
    g <- (img2 - min(img2)) * (1 / (max(img2)-min(img2)))
    b <- (img3 - min(img3)) * (1 / (max(img3)-min(img3)))
    col <- rgb(r, g, b)
    #dim(col) <- dim(r)
    return(as.raster(matrix(col, ncol=ncol(img1))))
    ##grid.raster(col, interpolate=FALSE)
}

## Normalize img to incidence angle
aNorm <- function(img, ang, band) {
    ## Previously calculated mean of available inc_angle
    mean.inc.angle <- 39.26871
    ## If inc_angle not available, assume mean (zero offset)
    if(ang %in% c(NA)) return(img)
    ## These are intercepts for a best-fit line for mean background
    ##  intensity vs inc_angle:
    intcpt <- c(-3.156, -15.563)[band]
    coef <- c(-0.46, -0.283)[band]
    ## Calculate where the mean inc_angle falls on best-fit line
    atmean <- intcpt + coef * mean.inc.angle 
    ## Calculate where this angle falls on best-fit line
    est <- intcpt + coef * ang 
    ## Subtract the difference to normalize to mean inc_angle
    img <- img - (est - atmean)
    return(img)
}


## This will get the features from idn that are used in the LR
getFeat <- function(tbl, idn) {
    ## Get image and other items needed to calculate features
    inc_angle <- getJinc_angle(tbl,idn)
    #b1 <- getJImg(tbl, 1, idn)
    b1 <- aNorm(getJImg(tbl, 1, idn), inc_angle, 1)
    b1.max.position <- findCenter(b1)
    b1.tar <- zoom(b1, b1.max.position, 3)
    #b2 <- getJImg(tbl, 2, idn)
    b2 <- aNorm(getJImg(tbl, 2, idn), inc_angle, 2)
    b2.tar <- zoom(b2, b1.max.position, 3)
    
    
    
    ## Now calculate imagefeatures
    res <-  data.frame(is_iceberg = NA)
    res$band.1.max.position.r <- b1.max.position[[1]][1]
    res$band.1.max.position.c <- b1.max.position[[1]][2]
    res$band.1.mean <- mean(b1)
    res$band.2.mean <- mean(b2)
    res$band.1.bg.mean <- mean(b1[brdmask])
    res$band.2.bg.mean <- mean(b2[brdmask])    
    res$band.1.var <- var(as.vector(b1))
    res$band.1.tar.var <- var(as.vector(b1.tar))
    res$band.1.tb.var.r <- res$band.1.tar.var / var(b1[brdmask])
    res$band.2.var <- var(as.vector(b2))
    res$band.2.tar.var <- var(as.vector(b2.tar))    
    res$band.2.tb.var.r <- res$band.2.tar.var / var(b2[brdmask])
    res$band.1.max <- max(b1)
    res$band.2.max <- max(b2)
    res$band.1.tar.mean <- mean(b1.tar)
    res$band.2.tar.mean <- mean(b2.tar)
    res$band.1.tar.ghs.mean <- mean(kern(b1.tar, k.gradh)^2)
    res$band.2.tar.ghs.mean <- mean(kern(b2.tar, k.gradh)^2)
    res$band.1.tar.gvs.mean <- mean(kern(b1.tar, k.gradv)^2)
    res$band.2.tar.gvs.mean <- mean(kern(b2.tar, k.gradv)^2)
    res$band.1.tb.mean.dif <- res$band.1.tar.mean - mean(b1[brdmask])
    res$band.2.tb.mean.dif <- res$band.2.tar.mean - mean(b2[brdmask])
    res$tar.sum.gs.sum.mean <- mean(kern(b1.tar+b2.tar, k.gradh)^2
                                + kern(b1.tar+b2.tar, k.gradv)^2)
    res$tb.var.r.sum <- res$band.1.tb.var.r + res$band.2.tb.var.r
    res$tar.cor <- cor(b1[stripBackground(b1)], b2[stripBackground(b1)])
    res$tar.mean.dif <- res$band.1.tar.mean - res$band.2.tar.mean
    res$tar.ghs.dif <- res$band.1.tar.ghs.mean - res$band.2.tar.ghs.mean
    res$tar.gvs.dif <- res$band.1.tar.gvs.mean - res$band.2.tar.gvs.mean
    res$tb.mean.dif.dif <- res$band.1.tb.mean.dif - res$band.2.tb.mean.dif
    res$inc_angle <- inc_angle
    ## only extract label if this is training data
    if(tbl=='train') {
        res$is_iceberg <- as.numeric(getJAtr(tbl, 'is_iceberg', p('idn=', idn)))
    } else {
        res$is_iceberg <- NA
    }

    ## Return features
    return(res)
}

## Log Loss Function
##   http://www.exegetic.biz/blog/2015/12/making-sense-logarithmic-loss/
logloss <- function(act, pred) {
    eps <- 1e-15
    pred <- pmin(pmax(pred, eps), 1-eps)
    -(sum(act * log(pred) + (1 - act) * log(1 - pred))) / length(act)
}


## V-Fold Cross Validation(X, features, V)
VFXV <- function(X, f, t, V, plotroc) {
    test.perf=data.frame(actual=numeric(), pred=numeric())
    fsize <- floor(rlen / V)
    LogLoss = data.frame(train=NA, test=NA)
    ## Loop through each fold
    for(v in 1:V) {
        r.first <- ((v - 1) * fsize) + 1 ## calc. starting index for test
        if(v == V) { ## calc. stopping index for test
            r.last <- rlen ## if last fold
        } else {
            r.last <- v * fsize ## if not last fold
        }
        records.train <- records[-c(r.first:r.last)] ## training set
        records.test <- records[r.first:r.last]      ## testing set
        ## Build training features
        Xr <- X[records.train, c(f,t)]
        model <- glm(formula = is_iceberg ~ .,
                     data = Xr,
                     family = binomial(logit) )
        ## Estimate probabilities & error for training data
        pr.train <- suppressWarnings(predict(model, Xr, type='response'))
        LogLoss[v,'train'] <- logloss(Xr$is_iceberg, pr.train)
        ## Estimate probabilities for test data
        Xs <- X[records.test, c(f, t)]
        pr.test <- suppressWarnings(predict(model, Xs, type='response'))
        LogLoss[v,'test'] <- logloss(Xs$is_iceberg, pr.test)
        test.perf <- rbind(test.perf, data.frame(actual=Xs[,t], pred=pr.test))
    }
    ## Plot a Receiver Operator Characteristics graph if requested
    if (plotroc == TRUE) {
        png('ROC_Results.png')
        roc.res <- roc(test.perf$actual, test.perf$pred, plot=FALSE, auc=TRUE)
        plot(roc.res, mar=c(5,4,4,2))
        title(main=p('Receiver Operator Curve, AUC = ', roc.res$auc))
        dev.off()
    }
    return(as.data.frame(t(colMeans(LogLoss))))
}



## GLOBAL VARIABLES




## This mask can be used to extract the border contents of an image
brdmask <- matrix(
(function (x) 1:(75*75)<=x*75 |
              1:(75*75)>(75-x)*75 |
              1:(75*75)%%75<=x |
              1:(75*75)%%75>(75-x))
(10)  # The width of the border
,75)

## Kernels
k.smooth <- matrix(rep(1/9, 9), 3, byrow=TRUE)
k.sharp <- matrix(c(0, -1, 0, -1, 5, -1, 0, -1, 0), 3, byrow=TRUE)
k.edgh <- matrix(c(0, 0, 0, -1, 2, -1, 0, 0, 0), 3, byrow=TRUE)
k.edgv <- matrix(c(0, -1, 0, 0, 2, 0, 0, -1, 0), 3, byrow=TRUE)
k.gradh <- matrix(rep(c(-1, 0, 1), 3), 3, byrow=TRUE)
k.gradv <- matrix(rep(c(-1, 0, 1), 3), 3, byrow=FALSE)

