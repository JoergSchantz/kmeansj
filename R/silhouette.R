#' Silhouette Score
#'
#' @param data
#' @param fit
#'
#' @return
#' @export
silhouette <- function( data, fit ) {
  # number data points
  n <- nrow( data )

  # number of clusters
  k <- nrow( fit$centroids )

  # distances of each point to another
  all_distances <- as.matrix( dist( data ) )
  all_distances <- all_distances[]

  # calculate individual silhouette scores
  sil <- sapply( 1:n,
                 function( i ) {
                   current_cluster <- fit$clusters[i]

                   # calculate average within cluster distance of current observation
                   avrg_within_dist <- mean( all_distances[fit$clusters == current_cluster, i] )

                   # calculate average "within cluster distance" of obsv. in every other cluster
                   avrg_neighbour_dist <- sapply( c( 1:k )[-current_cluster],
                                                  function( c ) {
                                                    mean( all_distances[fit$clusters == c, i] )
                                                    } )

                   # get nearest cluster by "within distance"
                   nearest_neighbour_dist <- min( avrg_neighbour_dist )

                   # calculate actual silhouette score
                   ( nearest_neighbour_dist - avrg_within_dist ) / max( nearest_neighbour_dist, avrg_within_dist )
                   } )

  mean( sil )
}
