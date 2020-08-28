######### CREATE TRAIN TABLE #####################

fname.in = 'train/train.json'             # choose raw downloaded json file
fname.out = 'train/train_formatted.json'  # choose name for cleaned file (can be same if replace)
wd <- '/home/john/Documents/IU_Cyber/17F-B565/project/iceberg/'
system(paste(paste(wd,'clean.sh',sep=''), fname.in, fname.out))

## Connect to mysql
library('RMySQL')
mydb <- dbConnect(MySQL(), user='john', password='', host='127.0.0.1')
## Create the database
dbSendQuery(mydb, 'DROP DATABASE IF EXISTS iceberg')
dbSendQuery(mydb, 'CREATE DATABASE iceberg')
dbSendQuery(mydb, 'USE iceberg')
## Create the table
dbSendQuery(mydb, 'CREATE TABLE train (idn INT AUTO_INCREMENT NOT NULL UNIQUE, img JSON, PRIMARY KEY (idn))')
## Populate the table
dbSendQuery(mydb, paste('LOAD DATA LOCAL INFILE', paste("'", wd, "train/train_formatted.json'", sep=''), 'INTO TABLE train (img)'))
## Clean up
dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)



######### CREATE TEST TABLE #####################

fname.in = 'test/test.json'             # choose raw downloaded json file
fname.out = 'test/test_formatted.json'  # choose name for cleaned file (can be same if replace)
wd <- '/home/john/Documents/IU_Cyber/17F-B565/project/iceberg/'
system(paste(paste(wd,'clean.sh',sep=''), fname.in, fname.out))

## Connect to mysql
library('RMySQL')
mydb <- dbConnect(MySQL(), user='john', password='', host='127.0.0.1')
## Connect to the database
dbSendQuery(mydb, 'USE iceberg')
## Create the table
dbSendQuery(mydb, 'CREATE TABLE test (idn INT AUTO_INCREMENT NOT NULL UNIQUE, img JSON, PRIMARY KEY (idn))')
## Populate the table
dbSendQuery(mydb, paste('LOAD DATA LOCAL INFILE', paste("'", wd, "test/test_formatted.json'", sep=''), 'INTO TABLE test (img)'))
## Clean up
dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)
