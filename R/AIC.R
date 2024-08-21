#' AIC for k-means fit
#'
#' @param fit k-means object
#'
#' @return A double
#' @export
#'
#' @examples
kmeans_aic <- function( fit ) {
  m <- ncol( fit$centroids )
  n <- length( fit$clusters )
  k <- nrow( fit$centroids )
  D <- fit$totwss
  return( D + 2 * m * k )
}

#' Optimal K with AIC
#'
#' @param data a data.frame
#' @param max_k upper bound for k
#'
#' @return
#' @export
#'
#' @examples
opt_k <- function( data, max_k = 10, method = "hw" ) {
  # calculate max_k fits based on the chosen method and their AIC
  fits <- sapply( 1:max_k,
                  function( j ) {
                    # calculate fit for j clusters
                    fit <- kmeans_hobbit( data = data,
                                          k = j,
                                          method = method )

                    # calculate AIC for j
                    kmeans_aic( fit = fit )
                    } )

  # format output to a data.frame
  data.frame( k = 1:max_k, AIC = fits )
}
