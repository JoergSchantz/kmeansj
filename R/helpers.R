#' Euclidean Distance
#'
#' @param a point A
#' @param b point B
#' @noRd
euclid_dist <- function( a, b ) {
  sqrt( sum( ( a - b ) ^ 2 ) )
}

#' Assign all Points to Clusters
#'
#' @param data
#' @param centroids
#' @noRd
assign_clusters <- function( data, centroids ) {
  clusters <- sapply( 1:nrow( data ),
                      function( i ) {
                        distances <- apply( centroids,
                                            1,
                                            function( centroid ) {
                                              euclid_dist( data[i, ], centroid )
                                            }
                        )
                        which.min( distances )
                      }
  )
  return( clusters )
}

#' Assign Point to Cluster in MacQueen
#'
#' @param point
#' @param centroids
#' @noRd
assign_cluster <- function( point, centroids ) {
  distances <- apply( centroids,
                      1,
                      function( centroid ) euclid_dist( point, centroid ) )
  which.min( distances )
}

#' Update Centroids
#'
#' @param data
#' @param clusters
#' @param k
#' @noRd
update_centroids <- function( data, clusters, k ) {
  centroids <- sapply( 1:k,
                       function( i ) {
                         colMeans( data[clusters == i, , drop = FALSE] )
                       } )
  t( centroids )
}

#' Sum of Squares
#'
#' @param data
#' @noRd
ss <- function( data ) {
  sum( apply( data,
              2,
              function( c ) {
                ( c - mean( c ) ) ^ 2
              }))
}

#' Within Cluster Sum of Squares
#'
#' @param data
#' @param clusters
#' @noRd
wcss <- function( data, clusters ) {
  sapply( split( data, clusters ), ss )
}
