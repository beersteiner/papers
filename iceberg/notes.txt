This is just a notes file to keep track of how I'm doing things for later documentation.


Create small json file from large json file:
    'create_tiny.sh <infile> <outfile>' # grabs first 500K from large json file
    'clean.sh <infile> <outfile>'       # extract each json object from array
                                          and puts on a new line, ready for SQL import

Populate SQL tables with json data:
    CREATE TABLE tablename (idn INT AUTO_INCREMENT NOT NULL UNIQUE, img JSON, PRIMARY KEY (idn))
    LOAD DATA LOCAL INFILE '<path to json file>' INTO TABLE tablename (img)'

Example JSON queries in SQL:
    # gets 0th index of band_1 from image 'dfd5f913'
    select img->"$.band_1[0]" from train where img->"$.id"="dfd5f913" 

Using packages: RMySQL, rjson, ggplot2, glm function, 

Analysis:
    plotting stats: https://www.kaggle.com/muonneutrino/exploration-transforming-images-in-python
    
    grid.raster(rgb(bh,bv,br)) where bh,bv,br are components of image from 0 to 1 intensity
    
    info about Sentinel-1 imagery: https://sentinel.esa.int/web/sentinel/user-guides/sentinel-1-sar/product-overview/polarimetry
    
    SAR Speckle Filtering: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=917910
    
    More on Speckle Filtering: https://earth.esa.int/documents/653194/656796/Speckle_Filtering.pdf
    
    
    


Visualization in library('lattic'):
    May switch to this instead of image():
        levelplot(b1,
            panel=function(...) {
                panel.levelplot(...)
                panel.points(imgstats$band.1.max.position[[1]][1],
                             imgstats$band.1.max.position[[1]][2],
                             pch=1,cex=5,col='red')
            }
    Also can use wireframe() or contourplot() in same way
    )


BEST RUN INFO:

Feature Calculation Runtime: 137.072
Feature Selection Runtime: 48.607
Model Training Runtime: 0.078000000000003
Test Data Prediction Runtime: 763.731
Total Runtime: 949.488




'pROC' gives area under curve (AUC)
'ROSE' gives receiver operating characteristic (ROC)

    
