library(tidyverse)
library(cluster)
library(kmeansj)

# comparing clustering results to stats::kmeans()

df <- iris %>% select(Sepal.Length, Petal.Length)

hw_kmeansj <- kmeansj::kmeansj(df, k = 3, method = "hw")
hw_stats <- stats::kmeans(df, centers = 3, algorithm = "Hartigan-Wong")

hw_kmeansj$centroids
hw_stats$centers

mcq_kmeansj <- kmeansj::kmeansj(df, k = 3, method = "mcq")
mcq_stats <- stats::kmeans(df, centers = 3, algorithm = "MacQueen")

mcq_kmeansj$centroids
mcq_stats$centers

lloyd_kmeansj <- kmeansj::kmeansj(df, k = 3, method = "lloyd")
lloyd_stats <- stats::kmeans(df, centers = 3, algorithm = "Lloyd")

lloyd_kmeansj$centroids
lloyd_stats$centers

# comparing silhouette score to cluster::silhouette()

hw_cluster <- cluster::silhouette(hw_stats$cluster, dist(df))
mean( hw_cluster[, 3] )
kmeansj::silhouette(hw_kmeansj, df)

mcq_cluster <- cluster::silhouette(mcq_stats$cluster, dist(df))
mean( mcq_cluster[, 3] )
kmeansj::silhouette(mcq_kmeansj, df)

lloyd_cluster <- cluster::silhouette(lloyd_stats$cluster, dist(df))
mean( lloyd_cluster[, 3] )
kmeansj::silhouette(lloyd_kmeansj, df)
