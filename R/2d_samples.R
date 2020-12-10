#--------------------------------
#' Simulate a Uniform distribution on a frown
#'
#' \code{frown} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' Most functions have three tags: @param, @examples and @return.
#'
#'
#' @param
#' n the size of the sample
#'
#' @param
#' x0 x coordinate of the center of the head, default to \code{0}
#'
#' @param
#' y0 x coordinate of the center of the head, default to \code{0}
#'
#' @param  
#' main_radius the radius of the head, default to \code{1}
#'
#' @param
#' eye_radius the radius of the eye, default to \code{0.25}
#'
#' @param
#' mouth_radius the radius of the mouth, default to \code{0.7}
#'
#' @param
#' mouth_start the radius of the mouth, default to \code{0.05}
#'
#' @param
#' mouth_end the radius of the mouth, default to \code{0.45}
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @examples
#' mySample <- frown(5000)
#'
#' @seealso
#' \url{https://www.r-project.org} as well as \code{\link{circle}}
#' for products, \code{\link{cumsum}} for cumulative sums, and
#' \code{\link{colSums}}/\code{\link{rowSums}} marginal sums over
#' high-dimensional arrays.
#'
#' @family Two-dimensional sampling functions.
#'
# @aliases frown1 frown2 frown3
#
frown <-
function(n, x0 = 0, y0 = 0, main_radius = 1, eye_radius = 0.25,
        mouth_radius = 0.7, mouth_start = 0.05, mouth_end = 0.45) {
  total_length <- main_radius +
                  2 * eye_radius +
                  mouth_radius * (mouth_end - mouth_start)
  mouth_count  <- ceiling(
                          n * mouth_radius *
                          (mouth_end - mouth_start)
                          /
                          total_length
                          )
  eye_count    <- ceiling(n * eye_radius / total_length)
  main_count   <- n - mouth_count - 2 * eye_count

  main         <- circle(main_count, main_radius)
  left_eye     <- circle(eye_count, eye_radius, -0.4, 0.4)
  right_eye    <- circle(eye_count, eye_radius,  0.4, 0.4)
  mouth        <- circle(
                         mouth_count,
                         mouth_radius,
                         0,
                         -0.8,
                         mouth_start,
                         mouth_end
                         )
  me     <- list(
                 x = x0 + c(main$x, left_eye$x, right_eye$x, mouth$x),
                 y = y0 + c(main$y, left_eye$y, right_eye$y, mouth$y)
                 )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#---------------------------------
#' Simulate a Uniform distribution on a circle
#'
#' \code{circle} simulates points in the plan as if they
#' were distributed uniformly on a circle.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' Most functions have three tags: @param, @examples and @return.
#'
#' @param
#' n the size of the sample
#'
#' @param
#' r the radius of the circle, default to \code{1}
#'
#' @param
#' x0 x coordinate of the center of the head, default to \code{0}
#'
#' @param
#' y0 y coordinate of the center of the head, default to \code{0}
#'
#' @param
#' from the radius of the mouth, default to \code{0.05}
#'
#' @param
#' to the radius of the mouth, default to \code{0.45}
#'
#' @return A list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @seealso \url{https://www.r-project.org}
#'
#' @family Two-dimensionalsampling functions.
#'
#' @seealso \code{\link{frown}} for products,
#' \code{\link{cumsum}} for cumulative sums, and
#' \code{\link{colSums}}/\code{\link{rowSums}}
#' marginal sums over high-dimensional arrays.
#'
#' @aliases circle1 circle2 circle3
#'
#' @examples
#' mySample <- circle(5000)
#'
circle <- function(n, r = 1, x0 = 0, y0 = 0, from = 0, to = 1) {
  theta <- 2 * (from + stats::runif(n) * (to - from)) * pi
  x_pos <- r * cos(theta)
  y_pos <- r * sin(theta)

  me    <- list(
                x = x_pos + x0,
                y = y_pos + y0
                )
  class(me) <- append(class(me), "jointSample")
  return(me)
}
