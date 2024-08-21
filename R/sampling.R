#' K-means++ sampling
#'
#' @param data
#' @param k
#'
#' @return
#' @keywords internal
#'
#' @examples
kmpp <- function( data, k ) {
  n <- nrow( data )
  ini_id <- sample( n, 1 )
  centroids <- data[ini_id, , drop = FALSE]
  for ( j in 2:k ) {
    # calculate the distance from each point to the closest centroid
    dist_to_closest_centroid <- apply( data,
                                       1,
                                       function( obsv ) {
                                         dist_to_all_centroids <- apply( centroids,
                                                                         1,
                                                                         function( c ) {
                                                                           sum( ( obsv - c ) ^ 2 )
                                                                         } )
                                         min( dist_to_all_centroids )
                                       })
    # calculate the probabilities for each point
    probs <- dist_to_closest_centroid / sum( dist_to_closest_centroid )

    # sample next centroid based on probabilities
    ini_id <- sample( n, 1, prob = probs )
    new_centroid <- data[ini_id, , drop = FALSE]

    # add new centroid to list of centroids
    centroids <- rbind( centroids, new_centroid )
  }

  return( centroids )
}
