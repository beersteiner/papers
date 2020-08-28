suppressMessages(library('RMySQL'))
suppressMessages(library('rjson'))
suppressMessages(library('ggplot2'))
suppressMessages(library('corrplot'))
suppressMessages(source('reuse.r'))




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
    write('Calculating Statistics', stdout())
    pb <- txtProgressBar(style=3, file=stderr())
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
    ggsave(p(gsub('\\.','_',atr),'.png'))
    }
    ## Save statistics to file
    save(imgstats, file='imgstats.Rda')
}

##Plot background intensity vs inc_angle
png('b1_bg_intensity-ing_angle.png')
plot(imgstats$inc_angle, imgstats$band.1.bg.mean)
abline(lm(imgstats$band.1.bg.mean~imgstats$inc_angle), col='red')
title(main='Band 2 Intensity Dependence on Incidence Angle')
dev.off()
png('b2_bg_intensity-ing_angle.png')
plot(imgstats$inc_angle, imgstats$band.2.bg.mean)
abline(lm(imgstats$band.2.bg.mean~imgstats$inc_angle), col='red')
title(main='Band 2 Intensity Dependence on Incidence Angle')
dev.off()

## Plot correlation matrix
png('corr_matrix.png')
corrplot(cor(imgstats[!(imgstats$inc_angle %in% c(NA)),]),
         type='upper', order='hclust', tl.col='black', tl.srt=45,
         tl.cex=.7, main='Feature Correlation Matrix', mar=c(0,0,4,0))
garbage <- dev.off()


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
    layout(matrix(c(1:12),3,4,byrow=TRUE))
    par(oma=c(0,0,8,0), bg='white', mar=c(0,0,4,0)) ## create outer margin area for multi-figure label
    plot(raster(t(b1)[,75:1])); title(main='band_1') # plot HH image ... should be image(t(img))[,75:1]
    plot(raster(t(b2)[,75:1])); title(main='band_2') # plot HV image
    plot(t(colorize(b1, b2, b1/b2))[,75:1]); title(main='band_1=red\nband_2=green\nb1/b2=blue')
    symbols(
    75-(imgstats$band.1.max.position.r[r]), ##should be [2]
    75-(imgstats$band.1.max.position.c[r]), ##should be 1-[1]
    circles=5, add=TRUE, inches=FALSE)
    legend('topright', inset=0.05, c('band.1.max.position'), pch=c('O'), bg='white', cex=.5)
    plot(raster(t(b1+b2)[,75:1])); title(main='added (b1+b2)') # plot combined image
    ##plot(raster(t(b1-b2)[,75:1])); title(main='difference')
    plot(raster(t(kern(b1+b2, k.smooth))[,75:1])); title(main='smoothed')       
    plot(raster(t(kern(b1+b2, k.sharp))[,75:1])); title(main='sharpened')       
    plot(raster(t(kern(b1+b2, k.edgh))[,75:1] + t(kern(b1+b2, k.edgv))[,75:1])); title(main='edges')
    ##plot(raster()); title(main='vert. edges')
    gh <- kern(b1+b2,k.gradh); plot(raster(t(gh)[,75:1])); title(main='horiz. gradient')
    plot(raster(t(gh^2)[,75:1])); title(main='horiz. gradient squared')
    gv <- kern(b1+b2,k.gradv); plot(raster(t(gv)[,75:1])); title(main='vert. gradient')
    plot(raster(t(gv^2)[,75:1])); title(main='vert. gradient squared')
    plot(raster(t(gh^2+gv^2)[,75:1])); title(main='sum of\nsquared gradient for h/v')

    ##invisible(); grid.raster(colorize(b1, b2, b2/b1), interpolate=FALSE)
    ## label the multi-figure plot
    mtext(p('id = ', i, '\ninc_angle = ', a, '\nis_iceberg = ', ice), outer=TRUE, cex=1.5)
    # output the id, angle, and label on-screen
    write(p(i, ' / ', a, ' / ' , ice, ' ... "p" to save to file, "q" to quit'), stdout())
    opt <- readline() # creates pause so observer can decide what to do next

    ## Create options for this current multi-figure image
    if(opt == 'p') { # User can save this image to a file
        dev.copy(png, p(gsub('\\.','_',i),'.png'), width=480, height=600)
        dev.off()
    } else if (opt == 'q') { # User can break out of this loop
        break
    } # Other options?
}

setwd(oldwd)

disconnectdb(mydb)
