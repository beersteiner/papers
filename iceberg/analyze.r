library('RMySQL')
library('rjson')
library('ggplot2')


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

## Apply convolution to an image given a 9x9 kernel k
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


## MAIN

mydb <- connectdb()

oldwd <- getwd()
setwd(p(oldwd,'/analysis'))

## Get initial list of records to work with
rec.ice <- dbGetQuery(mydb, 'SELECT idn FROM train WHERE img->"$.is_iceberg"=1')[,1]
rec.boat <- dbGetQuery(mydb, 'SELECT idn FROM train WHERE img->"$.is_iceberg"=0')[,1]
records <- 1:(length(rec.ice)+length(rec.boat))
rlen <- length(records)


## Histograms

if(file.exists('imgstats.Rda')) {
    load('imgstats.Rda')
} else {
    ## Initialize storage for image statistics
    imgstats <- data.frame(
        'band_1_mean'=rep(NA, rlen),
        'band_1_var'=rep(NA, rlen),
        'band_1_max'=rep(NA, rlen),
        'band_1_max_position'=rep(NA, rlen),
        'band_2_mean'=rep(NA, rlen),
        'band_2_var'=rep(NA, rlen),
        'band_2_max'=rep(NA, rlen),
        'band_2_max_position'=rep(NA, rlen),
        'inc_angle'=rep(NA, rlen),
        'is_iceberg'=rep(NA, rlen)
                           )
    ## Get image statistics
    for(r in records) { options(warn=2)
        b1 <- getJImg('train', '1', r)
        imgstats$band_1_mean[r] <- mean(b1)
        imgstats$band_1_var[r] <- var(as.vector(b1))
        imgstats$band_1_max[r] <- max(b1)
        imgstats$band_1_max_position[r] <- which(t(b1)==imgstats$band_1_max[r])[1]
        b2 <- getJImg('train', '2', r)
        imgstats$band_2_mean[r] <- mean(b2)
        imgstats$band_2_var[r] <- var(as.vector(b2))
        imgstats$band_2_max[r] <- max(b2)
        imgstats$band_2_max_position[r] <- which(t(b2)==imgstats$band_2_max[r])[1]
        tmp <- getJAtr('train','inc_angle', p('idn=',r))
        if (tmp %in% c('na')) {
            imgstats$inc_angle[r] <- NA
        } else {
            imgstats$inc_angle[r] <- as.numeric(tmp)
        }
        imgstats$is_iceberg[r] <- as.numeric(getJAtr('train','is_iceberg', p('idn=',r)))
    }
    ## Make plots of statistics
    for(atr in names(imgstats)[!(names(imgstats) %in% c('is_iceberg'))]) {
    data <- imgstats[!(imgstats[,atr] %in% c(NA)),]
    ggplot(data, aes(x=data[,atr], group=is_iceberg, fill=as.factor(is_iceberg))) +
        geom_histogram(position='identity', alpha=.5, bins=50) +
        scale_fill_discrete(name='is_iceberg') +
        theme_bw(base_size=20) +
        labs(title=atr, x='values')
    ggsave(p(atr,'.png'))
    }

    ## Save statistics to file
    save(imgstats, file='imgstats.Rda')
}


## Loop through all images
for(r in sample(records)) {
    ## get attributes of the record
    i = getJAtr('train', 'id', p('idn=', r))             # id of image
    b1 = getJImg('train', '1', r)                        # band_1 matrix
    b2 = getJImg('train', '2', r)                        # band_2 matrix
    a = getJAtr('train', 'inc_angle', p('idn=', r))      # incidence angle
    ice = getJAtr('train', 'is_iceberg', p('idn=', r))   # iceberg label

    ## Create modified experimental/versions of images
    combined = b1 + b2
    b2m = b2 - mean(b2[brdmask])

    ## Create a multi-figure region for real-time plots
    ## (1x2 matrix), widths are each 2, height is 1
    layout(matrix(c(1:9),3,byrow=TRUE))#, c(2,2), c(2,2))
    par(oma=c(0,0,8,0), bg='white') ## create outer margin area for multi-figure label
    image(b1, main='band_1', axes=TRUE, zlim=c(min(b1,b2),max(b1,b2))) # plot HH image
    image(b2, main='band_2', axes=TRUE, zlim=c(min(b1,b2),max(b1,b2))) # plot HV image
    image(combined, main='combined', axes=TRUE) # plot combined image
    symbols(
        (imgstats$band_1_max_position[r] / 75) / 75,
        (imgstats$band_1_max_position[r] %% 75) / 75,
        circles=0.1, add=TRUE, inches=FALSE)
    legend('topright', inset=0.02, c('band_1_max_position'), pch=c('O'), bg='white', cex=.5)
    ## Plot a variety of processed images
    image(kern(combined, k.smooth), main='smoothed', axes=TRUE)       
    image(kern(combined, k.sharp), main='sharpened', axes=TRUE)       
    image(kern(combined, k.edgh), main='horiz. edges', axes=TRUE)
    image(kern(combined, k.edgv), main='vert. edges', axes=TRUE)
    image(kern(combined, k.gradh), main='horiz. gradient', axes=TRUE)
    image(kern(combined, k.gradv), main='vert. gradient', axes=TRUE)
    ## label the multi-figure plot
    mtext(p('id = ', i, '\ninc_angle = ', a, '\nis_iceberg = ', ice), outer=TRUE, cex=1.5)
    # output the id, angle, and label on-screen
    message(p(i, ' / ', a, ' / ' , ice, ' ... "p" to save to file, "q" to quit'))
    opt <- readline() # creates pause so observer can decide what to do next

    ## Create options for this current multi-figure image
    if(opt == 'p') { # User can save this image to a file
        dev.copy(png, p(i,'.png'), width=480, height=600)
        dev.off()
    } else if (opt == 'q') { # User can break out of this loop
        break
    } # Other options?
}

setwd(oldwd)

disconnectdb(mydb)
