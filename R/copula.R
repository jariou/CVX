#----------------------------------------------
#' copula
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param jointSample An object of class "jointSample" such as
#' created by a call to smile or frown
#'
#' @return The empirical copula model of thejont sample
#' input \deqn{\int_0^1xdx= \frac{x^2}{2} + C}
#' and
#' \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}}{%p(x) =
#' \lambda^x exp(-\lambda)/x!} for \eqn{x = 0, 1, 2, \ldots}.
#' @export
#'
#' @examples
#' copula(joint_sample)
copula <- function(joint_sample) {
  size  <- length(joint_sample$x)
  tmp_0 <- lapply(joint_sample, rank)
  tmp_1 <- lapply(
                  tmp_0,
                  "[",
                  order(joint_sample$x)
                  )
  me <- lapply(
               tmp_1,
               function(t) {
                            t / (size + 1)
                            }
               )
  me <- lapply(
               me,
               "[",
               rank(joint_sample$x)
               )
  class(me) <- append(class(me), "jointSample")
  class(me) <- append(class(me), "copula")
  return(me)
}
