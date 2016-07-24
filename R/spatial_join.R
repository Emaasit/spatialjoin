#' Join attributes from one feature to another based on the spatial relationship.
#'
#' @param x Join dataframe.
#' @param y Target dataframe.
#' @param xeast Easting in the Join dataframe.
#' @param xnorth Northing in the Join dataframe.
#' @param yeast Easting in the Target dataframe.
#' @param ynorth Northing in the Target dataframe.
#' @return The spatial join and attributes from dataframes \code{x} and \code{y}.

spatial_join <- function(x, y, xeast, xnorth, yeast, ynorth) {
  ## construct data frame d in which d[i,] contains information
  ## associated with the closest point in y to x[i,]
  xpos <- as.matrix(x[,c(xeast, xnorth)])
  xposl <- lapply(seq.int(nrow(x)), function(i) xpos[i,])
  ypos <- t(as.matrix(y[,c(yeast, ynorth)]))
  yinfo <- y[,! colnames(y) %in% c(yeast,ynorth)]

  get_match_and_dist <- function(point) {
    sqdists <- colSums((point - ypos)^2)
    ind <- which.min(sqdists)
    c(ind, sqrt(sqdists[ind]))
  }
  match <- sapply(xposl, get_match_and_dist)
  cbind(xpos, mindist=match[2,], yinfo[match[1,],])
}
