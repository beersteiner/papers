
## This is a function so it can be invoked easily from interactive R session
connectdb <- function() {
    ## Connect to database
    library('RMySQL')
    library('rjson')
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
    res <- matrix(fromJSON(as.character(tmp)), ncol=75)
    return(res)
}




mydb <- connectdb()

## Get initial list of records to work with
records <- dbGetQuery(mydb, 'SELECT idn FROM train')[,1]


## Loop through all images
for(r in records) {
    i = getJAtr('train', 'id', p('idn=', r))             # id of image
    b1 = getJImg('train', '1', r)                        # band_1 matrix
    b2 = getJImg('train', '2', r)                        # band_2 matrix
    a = getJAtr('train', 'inc_angle', p('idn=', r))      # incidence angle
    ice = getJAtr('train', 'is_iceberg', p('idn=', r))   # iceberg label
    ## Create a multi-figure region for real-time plots
    ## (1x2 matrix), widths are each 2, height is 1
    layout(matrix(c(1,2),1,byrow=TRUE), c(2,2), c(2))
    par(oma=c(0,0,5,0)) ## create outer margin area for multi-figure label
    image(b1, main='band_1', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2))) # plot HH image
    image(b2, main='band_2', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2))) # plot HV image
    ## lael the multi-figure plot
    mtext(p('id = ', i, '\ninc_angle = ', a, '\nis_iceberg = ', ice), outer=TRUE, cex=1.5)
    message(p(i, ' / ', a, ' / ' , ice, ' ... "p" to save to file, "q" to quit')) # output the id, angle, and label on-screen
    opt <- readline() # creates pause so observer can decide what to do next
    ## Create options for this current multi-figure image
    if(opt == 'p') { # User can save this image to a file
        png(p(i, '.png'), width=720, height=480)
        layout(matrix(c(1,2),1,byrow=TRUE), c(2,2), c(2))
        par(oma=c(0,0,5,0))
        image(b1, main='band_1', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2)))
        image(b2, main='band_2', axes=FALSE, zlim=c(min(b1,b2),max(b1,b2)))
        mtext(p('id = ', i, '\ninc_angle = ', a, '\nis_iceberg = ', ice), outer=TRUE, cex=1.5)
        dev.off()
    } else if (opt == 'q') { # User can break out of this loop
        break
    } # Other options?
}


disconnectdb(mydb)
