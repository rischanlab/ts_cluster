#install.packages("rgl")
#install.packages("geometry")
#http://clusteranalysiswithr.blogspot.kr/2014/05/clustering-series-2.html
library(rgl)
library(geometry)

## Given that most of the discriminating variance is in the first 3 or 4
## dimensions, why not plot the clusters in 3D.  Note how it separates into
## two clusters the cluster that is shown in 2D.  If you line up the 3D plot
## in the first and second labeled dimension, you will notice the 2D image

# Functions to draw the 2D and 3D hulls.
drawhullLinesXY <- function(j, hull, hpts, repmins, i) {
  rgl.lines(hull[hpts[j:(j + 1)], 1], hull[hpts[j:(j + 1)], 2], repmins, col = i + 
              1, lty = 2)
}
drawhullLinesYZ <- function(j, hull, hpts, repmins, i) {
  rgl.lines(repmins, hull[hpts[j:(j + 1)], 1], hull[hpts[j:(j + 1)], 2], col = i + 
              1, lty = 2)
}
drawhullLinesXZ <- function(j, hull, hpts, repmins, i) {
  rgl.lines(hull[hpts[j:(j + 1)], 1], repmins, hull[hpts[j:(j + 1)], 2], col = i + 
              1, lty = 2)
}

findHulls2D <- function(i, clustermembership, x, y, repmins, coordsflag) {
  hull <- cbind(x[clustermembership == i], y[clustermembership == i])
  hpts <- chull(hull)
  hpts <- c(hpts, hpts[1])
  if (coordsflag == 1) {
    sapply(1:(length(hpts) - 1), drawhullLinesXY, hull, hpts, repmins, i)
  } else if (coordsflag == 2) {
    sapply(1:(length(hpts) - 1), drawhullLinesYZ, hull, hpts, repmins, i)
  } else {
    sapply(1:(length(hpts) - 1), drawhullLinesXZ, hull, hpts, repmins, i)
  }
}

numClusters <- max(dbscanDTW$cluster)

# A function is created that will plot the dbscan clustering in 3D, and one
# that will plot the synthetic control classes for comparison.

# Because we set the the minimum points (MinPts) to 5, we know that all
# clusters have at least 4 points, and therefore have enough points to form
# a convex hull in 3D. Also, note that those points identified by 0s in
# `dbscanDTW$cluster` are noise or singletons, and therefore not in any
# cluster.
PlotClusters3D <- function(data, clusters, numClusters) {
  # Plot points as very small colored spheres.
  plot3d(data[, 1], data[, 2], data[, 3], xlab = "1st dim", ylab = "2nd dim", 
         zlab = "3rd dim", col = 1 + clusters)
  
  # Plot the 3D convex hulls with considerable transparency (alpha=.2)
  for (i in 1:numClusters) {
    Points3D <- cbind(data[clusters == i, 1], data[clusters == i, 2], data[clusters == 
                                                                             i, 3])
    ts.surf <- t(convhulln(Points3D))
    rgl.triangles(Points3D[ts.surf, 1], Points3D[ts.surf, 2], Points3D[ts.surf, 
                                                                       3], col = 1 + i, alpha = 0.2)
  }
  # Plot the 2D hulls on walls of bounding box.  Note the box has been equally
  # scaled, even though the range of each dimension from the MDS is
  # successively smaller.  This shows that in any combination of 2 dimensions,
  # the clusters overlap significantly
  sapply(1:numClusters, findHulls2D, clusters, data[, 1], data[, 2], rep(min(data[, 
                                                                                  3]), 2), 1)
  sapply(1:numClusters, findHulls2D, clusters, data[, 2], data[, 3], rep(min(data[, 
                                                                                  1]), 2), 2)
  sapply(1:numClusters, findHulls2D, clusters, data[, 1], data[, 3], rep(min(data[, 
                                                                                  2]), 2), 3)
  rgl.bbox()
}

## interactive 3D plot -- to get two of them, call open3d() twice
firstfocus <- open3d()
# Here are the actual class assignments in 3D and their 3D convex hulls
PlotClusters3D(MDSDdtw, classcolors, 6)
# Here we plot the dbscan clustering
secondfocus <- open3d()
PlotClusters3D(MDSDdtw, dbscanDTW$cluster, numClusters)