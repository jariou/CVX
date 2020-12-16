#---- BEGIN copMix ----------------------------------------
#' Mixture of copulas
#'
#' \code{copMix} takes n empirical copulas and as many 
#' wwights produces a mixture of copulas
#'
#' @param copulas list of copulas
#' @param weights the vector of weights
#'
#' @return A list with two vectors of x and y of the samples 
#'         coordinates. The two list members are also named 
#'         x and y and the list itself is of class type 
#'         \code{2dJointSample}.
#'
#' @export
#' @examples
#' Gaussian(5000, c(3,4), matrix(c(25, 8, 8, 36)) )
#'
#' @family Two-dimensionalsampling functions.
#'
#' @aliases copMix1 copMix2 copMix3
copMix <-
function(copulas, weights) {
#  sample_size  <- length(samples[[1]])
#  sample_index <- 1 + apply(outer(runif(sample_size), cumsum(weights), ">"), 1, sum)
#  sample_ids   <- runif(sample_size)
#  sample1_reps <- sample_ids < w1
#  sample2_reps <- !sample1_reps

#  me <- list(
#             x = c(
#                   sample1$x[sample1_reps], 
#                   sample2$x[sample2_reps]
#                   ),
#             y = c(
#                   sample1$y[sample1_reps], 
#                   sample2$y[sample2_reps]
#                   )
#             )
#  copula(me)
}
#---- END copMix ------------------------------------------

#---- BEGIN rMIE ------------------------------------------
#' Mixture of Independent Erlangs#'
#' \code{copMix} takes n empirical copulas and as many 
#' wwights produces a mixture of copulas
#'
#' @param sample_size list of copulas
#' @param dimension the vector of weights
#' @param scale list of copulas
#' @param weights the vector of weights
#' @param shapes list of copulas
#'
#' @return A list with two vectors of x and y of the samples 
#'         coordinates. The two list members are also named 
#'         x and y and the list itself is of class type 
#'         \code{2dJointSample}.
#'
#' @export
# @examples
# Gaussian(5000, c(3,4), matrix(c(25, 8, 8, 36)) )
#'
#' @family Two-dimensionalsampling functions.
#'
#' @aliases rMIE1 rMIE2 rMIE3
rMIE <- function(
                 sample_size,
                 dimension,
                 scale,
                 weights,
                 shapes
                 ) {
  components_count <- length(weights)
  sample_index     <- 1 + apply(
                                outer(
                                      runif(sample_size * components_count), 
                                      cumsum(weights),
                                      ">"
                                      ),
                                1,
                                sum
                                )

  sample_ids       <- runif(sample_size)
  sample1_reps     <- sample_ids # < w1
  sample2_reps     <- !sample1_reps

#  me <- list(
#             x = c(
#                   sample1$x[sample1_reps], 
#                   sample2$x[sample2_reps]
#                   ),
#             y = c(
#                   sample1$y[sample1_reps], 
#                   sample2$y[sample2_reps]
#                   )
#             )
  # copula(me)
}
#---- END rMIE --------------------------------------------