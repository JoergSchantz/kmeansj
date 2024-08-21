#' Cluster prediction on test data
#'
#' @param data unseen test data
#' @param fit fitted model
#'
#' @return
#' @export
#'
#' @examples
km_predict <- function( data, fit ) {
  pred <- assign_clusters( data, fit$centroids )

  # more to do
  # ...
}
