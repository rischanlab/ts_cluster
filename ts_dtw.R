### Clustering Time Series - Comparing Signals using DTW ###
### Rischan Mafrur ###
### January, 24 2015 ###
setwd("/Volumes/RISCHAN/githubprojects/ts_cluster")
ts <- read.table("synthetic_control.data.txt")
View(ts) #total data 600 rows with 60 columns values

### pick sample from dataset
n <- 10
s <- sample(1:100,n)
idx <- c(s, 100+s, 200+s, 300+s, 400+s, 500+s)
sample2 <- ts[idx,]
observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n))


View(sample2) # only 60 samples

#install.packages("dtw")
library(dtw)
distMatrix <- dist(sample2, method="DTW")
class(distMatrix)

## example data to test dist calculation
v1 <- c(3,4,5,6,7)
v2 <- c(1,2,3,4,5)
v3 <- rbind(v1,v2)
distv <- dist(v3,method="DTW")

distv #4.472136 using Euclidean #6 with DTW method
# euclidean equation -> 2^2 + 2^2 ... 5 x and then sqrt
sqrt((2^2)*5) # 4.472136 
#method is correct

## hierarchial clustering
hc <- hclust(distMatrix, method = "average")
plot(hc, labels=observedLabels, main="")

