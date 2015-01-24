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

Dcormat <- abs(cor(t(as.matrix(ts[, -(numcols +1)]))))
library(MASS)
library(fpc)
MDSDdtw <- isoMDS(Ddtw, k=3)$points
dbscanDTW <- dbscan(MDSDdtw, eps = 100, MinPts = 5, method = "raw")

classcolors <- ts[, numcols +1]
par(mfrow = c(1, 2))
plot(MDSDdtw[, 1], MDSDdtw[, 2], xlab = "First Dimension  Actual Classes", ylab = "Second Dimension", 
     type = "n")
points(MDSDdtw[, 1], MDSDdtw[, 2], pch = classcolors, col = classcolors)

plot(MDSDdtw[, 1], MDSDdtw[, 2], xlab = "First Dimension  DBSCAN Clustering", 
     ylab = "Second Dimension", col = 1 + dbscanDTW$cluster)



