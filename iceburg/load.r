## The purpose of this script is simply to automate the loading of the large iceburg json
##   files into SQL.


fname.in = 'train.json'             # choose raw downloaded json file
fname.out = 'train_formatted.json'  # choose name for cleaned file (can be same if replace)

wd <- '/home/john/Documents/IU_Cyber/17F-B565/project/iceburg/train/'
system(paste(paste(wd,'clean.sh',sep=''), fname.in, fname.out))

## Connect to mysql
library('RMySQL')
mydb <- dbConnect(MySQL(), user='john', password='', host='127.0.0.1')



## Create the database
dbSendQuery(mydb, 'DROP DATABASE IF EXISTS iceburg')
dbSendQuery(mydb, 'CREATE DATABASE iceburg')
dbSendQuery(mydb, 'USE iceburg')

## Create the table
dbSendQuery(mydb, 'CREATE TABLE train (idn INT AUTO_INCREMENT NOT NULL UNIQUE, img JSON, PRIMARY KEY (idn))')

## Populate the table
dbSendQuery(mydb, paste('LOAD DATA LOCAL INFILE', paste("'", wd, "train_formatted.json'", sep=''), 'INTO TABLE train (img)'))

## Clean up
dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)
