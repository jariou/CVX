#---- BEGIN my_cdf ----------------------------------------
# Simple implementation of empirical cdf
# Common Scale Factor Dependent Model
# Perfect negative Dependency Copula
# Perfect positive Dependency Copula
# Make a movie from going from one plot to another one
#' Generate the empirical copula from a joint random sample
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param x an object of type \code{2dJointSample}
#'
#' @return 
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sample as a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' my_cdf(frown(1000)$x)
#'
#' @aliases my_cdf1 my_cdf2 my_cdf3
my_cdf <-
function(x) {
  sx    <- sort(x)
  rl    <- rle(sx)

  l     <- rl$lengths
  ll    <- length(l)

  my_x  <- rep(rl$values, l)
  my_y  <- cumsum(rep(rep(1, ll), l))

  list(
       x = my_x, 
       y = my_y
       )
}
#---- END my_cdf ------------------------------------------

#---- BEGIN Exponential -----------------------------------
#' Exponential Distribution with Scale parameter
#'
#' \code{Exponential} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param params the size of the sample
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' Exponential(c(1000))
#'
#' @seealso \code{\link{Uniform}},\code{\link{Degenerate}},
#' \code{\link{circle}}, \code{\link{InversePareto}}, \code{\link{Pareto}}.
#'
#' @family Two-dimensional sampling functions.
#'
#' @aliases Exponential1 Exponential2 Exponential3
Exponential <-
function(params) {
  pdf <- function(x) {
                      dexp(x, rate = 1 / params)
                      }
  cdf <- function(x) {
                      pexp(x, rate = 1 / params)
                      }
  quantile <- function(p) {
                           qexp(p, rate = 1 / params)
                           }
  rand <- function(n) {
                       rexp(n, rate= 1 / params)
                       }
  param_names <- c("Theta")
  param_desc  <- c("Scale parameter")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}
#---- BEGIN Exponential -----------------------------------

#---- BEGIN Degenerate ------------------------------------
#' Degenerate Distribution with parameter
#'
#' \code{frown} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param params the size of the sample
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' Degenerate(c(1000))
#'
#' @seealso \code{\link{Uniform}},
#' \code{\link{Pareto}}, \code{\link{InversePareto}},
#' \code{\link{Exponential}}.
#'
#' @family Two-dimensional sampling functions.
#'
#' @aliases Degenerate1 Degenerate2 Degenerate3
Degenerate <-
function(params) {
  pdf <- function(x) {
                      dexp(x, rate = 1 / params)
                      }
  cdf <- function(x) {
                      pexp(x, rate = 1 / params)
                      }
  quantile <- function(p) {
                           qexp(p, rate = 1 / params)
                           }
  rand <- function(n) {
                       rep(params, n)
                       }
  param_names <- c("X0")
  param_desc  <- c("The single possible realized value")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}
#---- END Degenerate --------------------------------------

#---- BEGIN Uniform ---------------------------------------
#' Uniform Distribution defaults to \code{[0, 1]}
#'
#' \code{Uniform} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param params the size of the sample
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' Uniform(c(0, 1))
#'
#' @seealso \code{\link{Degenerate}},\code{\link{Pareto}},
#' \code{\link{Exponential}}, \code{\link{InversePareto}}.
#'
#' @family One-dimensional loss distributions.
#'
#' @aliases Uniform1 Uniform2 Uniform3
Uniform <-
function(params) {
  pdf <- function(x) {
                      dunif(x, params[1], params[2])
                      }
  cdf <- function(x) {
                      punif(x, params[1], params[2])
                      }
  quantile <- function(p) {
                           qunif(p, params[1], params[2])
                           }
  rand <- function(n) {
                       runif(n, params[1], params[2])
                       }

  param_names <- c("Min", "Max")
  param_desc  <- c("Minimum possible value", "Maximum possible value")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}
#---- END Uniform -----------------------------------------

#---- BEGIN Pareto ----------------------------------------
#' Pareto Distribution
#'
#' \code{frown} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param params the size of the sample
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' Pareto(c(1000, 3))
#'
#' @seealso \code{\link{InversePareto}},
#' \code{\link{Uniform}},#' \code{\link{Degenerate}}, 
#' \code{\link{Exponential}}.
#'
#' @family Two-dimensional sampling functions.
#'
#' @aliases Pareto1 Pareto2 Pareto3
Pareto <-
function(params) {
  alpha <- params[2]
  theta <- params[1]

  pdf <- function(x) {
                      alpha / theta / (1 + x / theta)^ (alpha + 1)
                      }
  cdf <- function(x) {
                      1 - (1 / (x / theta + 1))^alpha
                      }
  quantile <- function(p) {
                           theta * (
                                    1 / (
                                         (1 - p)^ (1 / alpha)
                                         )
                                    - 1
                                   )
                           }
  rand <- function(n) {
                       theta * (
                                1 / (
                                     (1 - runif(n))^ (1 / alpha)
                                     )
                                - 1
                                )
                       }

  param_names <- c("Theta", "Alpha")
  param_desc  <- c("Scale parameter", "Shape parameter")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}
#---- END Pareto ------------------------------------------

#---- BEGIN InversePareto ---------------------------------
#' Inverse Pareto Distribution
#'
#' \code{frown} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param params the size of the sample
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' InversePareto(c(1000, 3))
#'
#' @seealso \code{\link{Pareto}},\code{\link{Exponential}},
#' \code{\link{Uniform}}, \code{\link{Degenerate}}, 
#'
#' @family Two-dimensional sampling functions.
#'
#' @aliases InversePareto1 InversePareto2 InversePareto3
InversePareto <-
function(params) {
  tau <- params[2]
  theta <- params[1]

  pdf <- function(x) {
                      tau / theta / (1 + x / theta)^ (tau + 1)
                      }
  cdf <- function(x) {
                      1 - (1 / (x / theta + 1))^tau
                      }
  quantile <- function(p) {
                           theta * (
                                    1 / (
                                         (1 - p)^ (1 / tau)
                                         )
                                    - 1
                                   )
                           }
  rand <- function(n) {
                       theta * (
                                1 / (
                                     (1 - runif(n))^ (1 / tau)
                                     )
                                - 1
                                )
                       }

  param_names <- c("Theta", "Tau")
  param_desc  <- c("Scale parameter", "Shape parameter")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}
#---- END InversePareto -----------------------------------