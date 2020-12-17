#--------------------------------------------------
# Random realizations of Erlangs
#'
#' \code{rErlang} simulates points in the plan as if they
#' were distributed as a mixture of Independent Erlang
#' distributions with fixed scale parameter.
#'
#' @param sample_size size ofsample
#' @param scale commonscale parameter
#' @param shape vector of shape parameters
#
#' @return If l = length(shapes), then the function
#' returns a matrix of sample_size rows and
#' l columns.
#'
#' @export
rErlang <- function(sample_size, scale, shape) {
  rgamma(sample_size, shape, scale = scale)
}
#--------------------------------------------------
#' Random realization vectors of Independent Erlangs
#'
#' \code{rMIErlang} simulates points in the plan as if they
#' were distributed as a mixture of Independent Erlang
#' distributions with fixed scale parameter.
#'
#' @param sample_size size ofsample
#' @param scale commonscale parameter
#' @param weights vector of mixture weights
#' @param shapes vector of shape parameters
#
#' @return If l = length(shapes), then the function
#' returns a matrix of sample_size rows and
#' l columns.
#' @export
# Random realizations of Erlangs
rMErlang <- function(sample_size, scale, weights, shapes) {
  sample(shapes, size = sample_size, replace = T, prob = weights)
  rgamma(sample_size, shapes, scale = scale)
}
#--------------------------------------------------
#' Random realization vectors of Independent Erlangs
#'
#' \code{rIErlang} simulates points in the plan as if they
#' were distributed as a mixture of Independent Erlang
#' distributions with fixed scale parameter.
#'
#' @param sample_size size ofsample
#' @param scale commonscale parameter
#' @param weights vector of mixture weights
#' @param shapes vector of shape parameters
#
#' @return If l = length(shapes), then the function
#' returns a matrix of sample_size rows and
#' l columns.
#' @export
rIErlang <- function(sample_size, scale, shapes, weights) {
  dimension <- length(shapes)
  t(matrix(rgamma(sample_size * dimension, shapes, scale = scale), dimension))
}

#---- BEGIN rMIErlang -------------------------------------
#' Simulate a mixture of Independent Erlang distribution
#'
#' \code{rMIErlang} simulates points in the plan as if they
#' were distributed as a mixture of Independent Erlang
#' distributions with fixed scale parameter.
#'
#' @param sample_size size ofsample
#' @param scale commonscale parameter
#' @param weights vector of mixture weights
#' @param shapes vector of shape parameters
#
#' @return A list with two vectors of x and y of the samples
#'         coordinates. The two list members are also named
#'         x and y and the list itself is of class type
#'         \code{2dJointSample}.
#'
#' @export
# @examples
# rMIErlang(1000, 100, (1:4)/10, (1:4) * 100)
#'
#' @family Two-dimensionalsampling functions.
#'
#' @aliases rMIErlang1 rMIErlang2 rMIErlang3
rMIErlang <- function(
                      sample_size,
                      scale,
                      weights,
                      shapes) {
  dimension    <- dim(shapes)[2]
  mix_count    <- length(weights)
  sample_index <- 1 + apply(
                            outer(
                                  runif(sample_size),
                                  cumsum(weights),
                                  ">"
                                  ),
                            1,
                            sum
                            )
  values       <- t(
                    matrix(
                            rgamma(
                                    sample_size * dimension,
                                    shapes,
                                    scale = scale
                                    ),
                    dimension))

  list(index = sample_index, values = values)
}
#---- BEGIN Gaussian --------------------------------------
