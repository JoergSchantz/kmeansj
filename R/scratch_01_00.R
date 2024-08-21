#' K-means after Lloyd
#'
#' @param data
#' @param k
#' @param max_iter
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
kmeans_lloyd <- function( data, k, max_iter = 20, tol = 1e-4 ) {
  # choose k random data points as starting centroids
  # ini_id <- sample( 1:nrow( data ), k )
  # centroids <- data[ini_id, ]
  centroids <- kmpp( data, k )

  for ( iter in 1:max_iter ) {
    # assign clusters
    clusters <- assign_clusters( data, centroids )

    # update centroids
    new_centroids <- update_centroids( data, clusters, k )

    # check convergence
    if( sum( ( centroids - new_centroids ) ^ 2 ) < tol ) {
      break
    }

    centroids <- new_centroids
  }

  totss <- ss( data )
  wss <- wcss( data, clusters )
  totwss <- sum( wcss( data, clusters ) )
  btwss <- totss - totwss
  size <- sapply( split( data, clusters ), nrow )

  result <- list(
    clusters = clusters,
    centroids = centroids,
    totss = totss,
    wss = wss,
    totwss = totwss,
    btwss = btwss,
    size = size
    )

  return( result )
}

#' K-means after Hartigan & Wong
#'
#' @param data
#' @param k
#' @param max_iter
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
kmeans_hw <- function( data, k, max_iter = 20, tol = 1e-4 ) {
  # choose k random data points as starting centroids
  # ini_id <- sample( 1:nrow( data ), k )
  # centroids <- data[ini_id, ]
  centroids <- kmpp( data, k )

  for (iter in 1:max_iter ) {
    # assign clusters
    clusters <- assign_clusters( data, centroids )

    # update centroids
    new_centroids <- update_centroids( data, clusters, k )

    # compute total within-cluster sum of squares
    total_wcss <-sum( wcss( data, clusters ) )

    # reassign points based on minimizing WCSS
    for ( i in 1:nrow( data ) ) {
      current_cluster <- clusters[i]
      min_wcss <- total_wcss

      for ( j in 1:k ) {
        if ( j != current_cluster ) {
          new_clusters <- clusters
          new_clusters[i] <- j
          new_wcss <- sum( wcss( data, new_clusters ) )

          if ( new_wcss < min_wcss ) {
            min_wcss <- new_wcss
            clusters[i] <- j
          }
        }
      }
    }

    # update centroids after reassignment
    centroids <- update_centroids( data, clusters, k )

    # Check for convergence
    if ( sum( ( centroids - new_centroids ) ^ 2 ) < tol ) {
      break
    }
  }

  totss <- ss( data )
  wss <- wcss( data, clusters )
  totwss <- sum( wcss( data, clusters ) )
  btwss <- totss - totwss
  size <- sapply( split( data, clusters ), nrow )

  result <- list(
    clusters = clusters,
    centroids = centroids,
    totss = totss,
    wss = wss,
    totwss = totwss,
    btwss = btwss,
    size = size
  )

  return( result )
}

#' K-means after MacQueens
#'
#' @param data
#' @param k
#' @param max_iter
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
kmeans_mcq <- function( data, k, max_iter = 20, tol = 1e-4 ) {
  # choose k random data points as starting centroids
  #
  # ini_id <- sample( 1:nrow( data ), k )
  # centroids <- data[ini_id, ]
  centroids <- kmpp( data, k )

  clusters <- rep( 0, nrow( data ) )
  cluster_sizes <- rep( 0, k )

  for ( iter in 1:max_iter )  {
    for ( i in 1:nrow( data ) ) {
      cluster <- assign_cluster( data[i, ], centroids )
      clusters[i] <- cluster
      cluster_sizes[cluster] <- cluster_sizes[cluster] + 1
      centroids[cluster, ] <- update_centroid( centroids[cluster, ],
                                               data[i, ],
                                               cluster_sizes[cluster] )
    }

    # compute total within-cluster sum of squares
    total_wcss <- sum( wcss( data, clusters ) )

    # Check for convergence
    if (iter > 1 && abs(prev_wcss - total_wcss) < tol ) {
      break
    }

    prev_wcss <- total_wcss
  }

  totss <- ss( data )
  wss <- wcss( data, clusters )
  totwss <- sum( wcss( data, clusters ) )
  btwss <- totss - totwss
  size <- sapply( split( data, clusters ), nrow )

  result <- list(
    clusters = clusters,
    centroids = centroids,
    totss = totss,
    wss = wss,
    totwss = totwss,
    btwss = btwss,
    size = size
  )

  return( result )
}

#' General k-means function with all algo options
#'
#' @param data
#' @param k
#' @param max_iter
#' @param tol
#' @param method
#'
#' @return
#' @export
#'
#' @examples
kmeans_hobbit <- function( data, k, max_iter = 20, tol = 1e-4, method = "hw" ) {
  methods <- list(
    lloyd = kmeans_lloyd,
    mcq = kmeans_mcq,
    hw = kmeans_hw
  )

  methods[[method]]( data, k, max_iter, tol )
}
