#----------------------------------------------
#' The \code{copula} function
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param joint_sample
#' An object of class \code{jointSample} such as
#' created by a call to \code{smile} or \code{frown}
#'
#' @return The empirical copula model of the joint sample
#'
#' @examples sum(9,5)
#'
#' @seealso \url{https://www.r-project.org}
#'
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
