### Clustering Time Series - Signal Comparing DTW ###
### Rischan Mafrur ###
### January, 24 2015 ###
setwd("/Volumes/RISCHAN/githubprojects/ts_cluster")
ts <- read.table("synthetic_control.data.txt")
View(ts) #total data 600 rows with 60 columns values

classSize <- 100
numcols <- ncol(ts)
ts[,numcols +1 ] <- unlist(lapply(1:6, rep, classSize))
View(ts) 
# 600 rows data with 6 classes, each clas contain 100 rows data

#Generating DTW distance

Ddtw <- dtwDist(ts[,-(numcols+1)],method="DTW")
write.table(as.matrix(Ddtw),"Ddtw.txt", quote=FALSE, sep=" ", row.names=FALSE,col.names = FALSE)

