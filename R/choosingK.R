#' Optimal K with Silhouette
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
                    fit <- kmeansj( data = data,
                                    k = j,
                                    method = method )

                    # calculate SC for j
                    silhouette( fit, data )
                    } )

  # format output to a data.frame
  data.frame( k = 1:max_k, SC = fits )
}
