

connectdb <- function() {
    ## Connect to database
    library('RMySQL')
    library('rjson')
    db <- dbConnect(MySQL(), user='john', password='', host='127.0.0.1')
    dbSendQuery(db, 'USE iceberg')
    return(db)
}


disconnectdb <- function(db) {
    ## Clean up
    ##dbClearResult(dbListResults(mydb)[[1]])
    dbDisconnect(db)
}


getJVec <- function(tbl, key) {
    qry <- paste('SELECT img->"$.',key,'" FROM ', tbl, sep='')
    suppressWarnings(res <- dbGetQuery(mydb, qry)[,1])
    for(i in 1:length(res)) res[i] <- fromJSON(res[i])
    return(res)
}

getJImg <- function(tbl, bnd, id) {
    qry <- paste('SELECT img->"$.band_', bnd, '" FROM ', tbl,
                 ' WHERE img->"$.id"="', id,'"', sep='')
    suppressWarnings(tmp <- dbGetQuery(mydb, qry))
    res <- matrix(fromJSON(as.character(tmp)), ncol=75)
    return(res)
}




mydb <- connectdb()

## Get list of all images
imageids <- getJVec('train','id')
imageangles <- suppressWarnings(as.numeric(getJVec('train','inc_angle')))
imagelabels <- as.numeric(getJVec('train','is_iceberg'))

## Loop through all images
for(n in 1:length(imageids)) {
    i = imageids[n]
    b1 = getJImg('train', '1', i)      # band_1 matrix
    b2 = getJImg('train', '2', i)      # band_2 matrix
    a = imageangles[n]                 # angle of incidence
    ice <- imagelabels[n]               # 1 if iceberg, 0 if not
    layout(matrix(c(1,2),1,byrow=TRUE), c(2,2), c(2))
    par(oma=c(0,0,5,0))
    image(b1, main='band_1', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2)))
    image(b2, main='band_2', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2)))
    mtext(paste('id =', i, '\ninc_angle = ', a, '\nis_iceberg = ', ice), outer=TRUE, cex=1.5)
    message(paste(i, '/', a, '/' ,ice))
    opt <- readline()
    if(opt == 'p') {
        png(paste(i, '.png', sep=''), width=720, height=480)
        layout(matrix(c(1,2),1,byrow=TRUE), c(2,2), c(2))
        par(oma=c(0,0,5,0))
        image(b1, main='band_1', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2)))
        image(b2, main='band_2', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2)))
        mtext(paste('id =', i, '\ninc_angle = ', a, '\nis_iceberg = ', ice), outer=TRUE, cex=1.5)
        dev.off()
    } else if (opt == 'q') break
}


disconnectdb(mydb)
