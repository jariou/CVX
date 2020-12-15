#-------------------------------------------------
# Exponential Distribution with Scale parameter
#---- BEGIN plot.jointSample ------------------------------
#' Simulate a Uniform distribution on a frowning face
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
# @examples
# Exponential(c(1000))
#'
#' @seealso \code{\link{triangle}},\code{\link{smile}},
#' \code{\link{circle}}, \code{\link{pizza}}, \code{\link{square}}.
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
