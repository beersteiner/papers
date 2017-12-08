library('RMySQL')
library('rjson')
library('ggplot2')
source('reuse.r')




## MAIN

mydb <- connectdb()

oldwd <- getwd()
setwd(p(oldwd,'/analysis'))

## Get initial list of record idns
records <- dbGetQuery(mydb, 'SELECT idn FROM train')[,1]#[1:100]


rlen <- length(records)


## Histograms
if(file.exists('imgstats.Rda')) {
    load('imgstats.Rda')
} else {
    message('Calculating Statistics')
    pb <- txtProgressBar(style=3)
    ## Initialize storage for image statistics
    imgstats <- data.frame()
    ## Get image statistics
    for(r in records) { #options(warn=2)
        setTxtProgressBar(pb, r/rlen)
        imgstats <- rbind(imgstats, getFeat('train', r))
    }
    close(pb)
    ## Make plots of statistics
    skp <- c('band.1.max.position.r','band.1.max.position.c','is_iceberg')
    for(atr in names(imgstats)[!(names(imgstats) %in% skp)]) {
    data <- imgstats[!(imgstats[,atr] %in% c(NA)),]
    ggplot(data, aes(x=data[,atr], group=is_iceberg, fill=as.factor(is_iceberg))) +
        geom_histogram(position='identity', alpha=.5, bins=50) +
        scale_fill_discrete(name='is_iceberg') +
        theme_bw(base_size=20) +
        labs(title=atr, x='values')
    ggsave(p(atr,'.png'))
    }
    ## Save statistics to file
    #save(imgstats, file='imgstats.Rda')
}


## Loop through all images
for(r in records) {
    ## get attributes of the record
    i = getJAtr('train', 'id', p('idn=', r))             # id of image
    b1 = getJImg('train', '1', r)                        # band_1 matrix
    b2 = getJImg('train', '2', r)                        # band_2 matrix
    a = getJAtr('train', 'inc_angle', p('idn=', r))      # incidence angle
    ice = getJAtr('train', 'is_iceberg', p('idn=', r))   # iceberg label
    ## Create modified experimental/versions of images
        
    ## Create a multi-figure region for real-time plots
    ## (1x2 matrix), widths are each 2, height is 1
    layout(matrix(c(1:16),4,4,byrow=TRUE))
    par(oma=c(0,0,8,0), bg='white') ## create outer margin area for multi-figure label
    image(t(b1)[,75:1], main='band_1', axes=TRUE, zlim=c(min(b1,b2),max(b1,b2))) # plot HH image ... should be image(t(img))[,75:1]
    image(t(b2)[,75:1], main='band_2', axes=TRUE, zlim=c(min(b1,b2),max(b1,b2))) # plot HV image
    image(t(b1+b2)[,75:1], main='added (b1+b2)', axes=TRUE) # plot combined image
    symbols(
        (imgstats$band.1.max.position.c[r] / 75), ##should be [2]
        1-(imgstats$band.1.max.position.r[r] / 75), ##should be 1-[1]
        circles=0.1, add=TRUE, inches=FALSE)
    legend('topright', inset=0.02, c('band.1.max.position'), pch=c('O'), bg='white', cex=.5)
    image(t(b1-b2)[,75:1], main='difference', axes=TRUE)
    ## Plot a variety of processed images
    image(t(kern(b1+b2, k.smooth))[,75:1], main='smoothed', axes=TRUE)       
    image(t(kern(b1+b2, k.sharp))[,75:1], main='sharpened', axes=TRUE)       
    image(t(kern(b1+b2, k.edgh))[,75:1], main='horiz. edges', axes=TRUE)
    image(t(kern(b1+b2, k.edgv))[,75:1], main='vert. edges', axes=TRUE)
    gh <- kern(b1+b2,k.gradh); image(gh, main='horiz. gradient', axes=TRUE)
    image(t(gh^2)[,75:1], main='horiz. gradient squared', axes=TRUE)
    gv <- kern(b1+b2,k.gradv); image(gv, main='vert. gradient', axes=TRUE)
    image(t(gv^2)[,75:1], main='vert. gradient squared', axes=TRUE)
    image(t(gh^2-gv^2)[,75:1], main='difference of squared gradient for h/v', axes=TRUE)

    ##invisible(); grid.raster(colorize(b1, b2, b2/b1), interpolate=FALSE)
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
