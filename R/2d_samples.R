#### BEGIN frown ##########################################
#' Draws points uniformly distributed on a frowning face
#'
#' \code{frown} simulates points in the plan
#' as if they were distributed uniformly on
#' the drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param n the size of the sample
#' @param x0 x coordinate of the center of the head,
#'        default to \code{0}
#' @param y0 y coordinate of the center of the head,
#'        default to \code{0}
#' @param main_radius the radius of the head, default
#'        to \code{1}
#' @param eye_radius the radius of the eye, default
#'        to \code{0.25}
#' @param mouth_radius the radius of the mouth,
#'        default to \code{0.7}
#' @param mouth_start number between \code{0} and \code{1}
#'        representing the fraction of \code{(2xPi)} angle
#'        at which the mouth stroke begins, default to
#'        \code{0.05}
#' @param mouth_end number between \code{0} and \code{1}
#'        representing the fraction of \code{(2Pi)} angle
#'        at which the mouth stroke ends, default to
#'        \code{0.45}
#'
#' @return a list with two vectors of x and y of the
#'         samples coordinates. The two list members
#'         are also named x and y and the list itself
#'         is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' frown(5000)
#'
#' @family Two-dimensional sampling functions
#'
#' @aliases frown1 frown2 frown3
frown <-
function(
          n,
          x0           = 0,
          y0           = 0,
          main_radius  = 1,
          eye_radius   = 0.25,
          mouth_radius = 0.7,
          mouth_start  = 0.05,
          mouth_end    = 0.45
          ) {
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
  me            <- list(
                 x = x0 + c(main$x, left_eye$x, right_eye$x, mouth$x),
                 y = y0 + c(main$y, left_eye$y, right_eye$y, mouth$y)
                 )
  class(me) <- append(class(me), "2DJointSample")
  return(me)
}
#### END frown ############################################

#### BEGIN smile ##########################################
#' Draws points uniformly distributed on a smiling face
#'
#' \code{smile} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a smiling face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param n the size of the sample
#' @param x0 x coordinate of the center of the head,
#'        default to \code{0}
#' @param y0 y coordinate of the center of the head,
#'        default to \code{0}
#' @param main_radius the radius of the head, default
#'        to \code{1}
#' @param eye_radius the radius of the eye, default to
#'        \code{0.25}
#' @param mouth_radius the radius of the mouth, default
#'        to \code{0.7}
#' @param mouth_start number between \code{0} and \code{1}
#'        representing the fraction of \code{(2xPi)} angle
#'        at which the mouth stroke begins, default to
#'        \code{0.55}
#' @param mouth_end number between \code{0} and \code{1}
#'        representing the fraction of \code{(2Pi)} angle
#'        at which the mouth stroke ends, default to
#'        \code{0.95}
#'
#' @return a list with two vectors of x and y of the
#'         samples coordinates. The two list members
#'         are also named x and y and the list itself
#'         is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' smile(5000)
#'
#' @family Two-dimensional sampling functions
#'
#' @aliases smile1 smile2 smile3
smile <-
function(
         n,
         x0           = 0,
         y0           = 0,
         main_radius  = 1,
         eye_radius   = 0.25,
         mouth_radius = 0.7,
         mouth_start  = 0.55,
         mouth_end    = 0.95
        ) {
  total_length <- main_radius +
                  2 * eye_radius +
                  mouth_radius * (mouth_end - mouth_start)

  mouth_count  <- ceiling(
                          n * mouth_radius * (mouth_end - mouth_start)
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
                        0,
                        mouth_start,
                        mouth_end
                        )

  me  <-   list(
                x = x0 + c(main$x, left_eye$x, right_eye$x, mouth$x),
                y = y0 + c(main$y, left_eye$y, right_eye$y, mouth$y)
                )
  class(me) <- append(class(me), "2DJointSample")
  return(me)
}
#### END smile ############################################

#### BEGIN circle #########################################
#' Draws points uniformly distributed on a circle arc
#'
#' \code{circle} simulates points in the plan as if they
#' were distributed uniformly on a circle arc.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param n the size of the sample
#' @param r the radius of the circle, default to \code{1}
#' @param x0 x coordinate of the center of the circle,
#'        default to \code{0}
#' @param y0 y coordinate of the center of the circle,
#'        default to \code{0}
#' @param from starting point of the arc as a fraction of
#'        the whole circumference starting from the
#'        rightmost point and drawing the circle in a
#'        counter-clockwise direction, default to \code{0}
#' @param to ending point of the arc as a fraction of the
#'        whole circumference starting from the rightmost
#'        point and drawing the circle in a counter-
#'        clockwise direction, default to \code{0}
#'
#' @return A list with two vectors of x and y of the
#'         samples coordinates. The two list members are
#'         also named x and y and the list itself is of
#'         class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' circle(5000)
#'
#' @family Two-dimensional sampling functions
#'
#' @aliases circle1 circle2 circle3
circle <- function(
                    n,
                    r    = 1,
                    x0   = 0,
                    y0   = 0,
                    from = 0,
                    to   = 1
                    ) {
  theta <- 2 * (from + runif(n) * (to - from)) * pi
  x_pos <- r * cos(theta)
  y_pos <- r * sin(theta)

  me    <- list(
                x = x_pos + x0,
                y = y_pos + y0
                )
  class(me) <- append(class(me), "2DJointSample")
  return(me)
}
#### BEGIN circle #########################################

#### BEGIN pizza ##########################################
#' Draws points uniformly distributed on a pizza slice
#'
#' \code{pizza} simulates points in the plan as if they
#' were distributed uniformly inside a pizza slice.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param n the size of the sample
#' @param r the radius of the pizza pie, default to
#'        \code{1}
#' @param hole the radius of the hole in the center of the
#'        pizza, default to \code{0}
#' @param x0 x coordinate of the center of the pizza pie,
#'        default to \code{0}
#' @param y0 y coordinate of the center of the pizza pie,
#'        default to \code{0}
#' @param from starting point of the arc as a fraction of
#'        the whole circumference starting from the
#'        rightmost point and drawing the circle in a
#'        counter-clockwise direction, default to \code{0}
#' @param to ending point of the arc as a fraction of the
#'        whole circumference starting from the rightmost
#'        point and drawing the circle in a counter-
#'        clockwise direction, default to \code{0}
#'
#' @return A list with two vectors of x and y of the
#'         samples coordinates. The two list members are
#'         also named x and y and the list itself is of
#'         class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' pizza(5000)
#'
#' @family Two-dimensional sampling functions
#'
#' @aliases pizza1 pizza2 pizza3
pizza <-
function(
          n,
          r    = 1,
          hole = 0,
          x0   = 0,
          y0   = 0,
          from = 0,
          to   = 1
          ) {
  radius_pos  <- sqrt(
                      runif(n) *
                      (r^2 - hole^2) +
                      hole^2
                      )

  theta <- 2 * (from + runif(n) * (to - from)) * pi
  x_pos <- radius_pos * cos(theta)
  y_pos <- radius_pos * sin(theta)

  me <- list(
             x = x_pos + x0,
             y = y_pos + y0
             )
  class(me) <- append(class(me), "2DJointSample")
  return(me)
}
#### END pizza ############################################

#### BEGIN square #########################################
#' Draws points uniformly distributed on the perimeter
#' of a square
#'
#' \code{square} simulates points in the plan as if they
#' were distributed uniformly on the perimeter of a square.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param n the size of the sample
#'
#' @return A list with two vectors of x and y of the
#'         samples coordinates. The two list members are
#'         also named x and y and the list itself is of
#'         class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' square(5000)
#'
#' @family Two-dimensional sampling functions
#'
#' @aliases square1 square2 square3
square <-
function(n) {
  side <- ceiling(runif(n) * 4)
  x <- runif(n)
  y <- runif(n)
  x[side == 1] <- 0
  x[side == 3] <- 1
  y[side == 2] <- 0
  y[side == 4] <- 1
  
  me <- list(
             x = x,
             y = y
             )
  class(me) <- append(class(me), "2DJointSample")
  return(me)
}
#### END square ###########################################

#### BEGIN triangle #######################################
#' Draws points uniformly distributed inside a triangle
#'
#' \code{triangle} simulates points in the plan as if they
#' were distributed uniformly on the inside of a triangle.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param n the size of the sample
#'
#' @return A list with two vectors of x and y of the
#'         samples coordinates. The two list members are
#'         also named x and y and the list itself is of
#'         class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' triangle(5000)
#'
#' @family Two-dimensional sampling functions
#'
#' @aliases triangle1 triangle2 triangle3
triangle <-
function(n) {
  y  <- 1 - sqrt(runif(n))

  me <- list(
             x = y / 2 + runif(n) * (1 - y),
             y = y
             )
  class(me) <- append(class(me), "2DJointSample")
  return(me)
}
#### END triangle #########################################

#---- BEGIN Gaussian --------------------------------------
#' Simulate a joint normal distribution
#'
#' \code{Gaussian} simulates points in the plan as if they
#' were distributed as joint Normal random variables.
#'
#' @param n the size of the sample
#' @param mu  the vector of means
#' @param sigmasq the covariance matrix
#' @param rho correlation coefficient
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
#' @aliases Gaussian1 Gaussian2 Gaussian3
Gaussian <-
function(n, mu, sigmasq, rho) {
  return(0)
}
#---- END Gaussian ------------------------------------------

#### BEGIN plot.2DJointSample #############################
#' Plot a two-dimensional joint sample
#'
#' \code{plot.2DJointSample} plots points in the plan
#' from objects of type \code{2DJointSample}
#'
#' @param x the size of the sample
#' @param pch the size of the sample
#' @param cex the size of the sample
#' @param all the size of the sample
#' @param ... the size of the sample
#'
#' @return a list with two vectors of x and y of the
#'         samples coordinates. The two list members are
#'         also named x and y and the list itself is of
#'         class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' plot(frown(5000))
#'
#' @family Methods for 2dJointSample class
#'
#' @aliases plot.2DJointSample1 plot.2DJointSample2 plot.2DJointSample3
plot.2DJointSample <-
function(
         x,
         pch = 20,
         cex = 0.1,
         all = F,
         ...
         ) {
  if (all) {
    # Setup the 4 plot canvases
    par(
        mfrow = c(2, 2),
        mar   = c(0, 0, 0, 0),
        mai   = c(0, 0, 0, 0)
        )

    # Plain X-Y plot of the sample
    plot.default(
                 x    = x,
                 pch  = 20,
                 cex  = 0.1,
                 tcl  = 0,
                 xlab = "",
                 ylab = "",
                 xaxt = "n",
                 yaxt = "n",
                 ...
                 )

    # Plot flipped CDF of Y marginal
    tmp_y <- my_cdf(x$y)
    plot(
         list(x = tmp_y$y, y = tmp_y$x),
         pch  = 20,
         cex  = 0.1,
         tcl  = 0,
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = "n",
         ...
         )

    # Plot CDF of X marginal
    plot(
         my_cdf(x$x),
         pch  = 20,
         cex  = 0.1,
         tcl  = 0,
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = "n",
         ...
         )

    # Plot of the copula
    plot(
         copula(x),
         xaxt = "n",
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = "n",
         ...
         )
  }
  else {
        plot.default(
                     x   = x,
                     pch = 20,
                     cex = 0.1,
                     ...
                     )
  }
}
#### END plot.2DJointSample ###############################

#---- BEGIN print.2DJointSample ---------------------------
#' Print a two-dimensional joint sample
#' 
#' \code{print.2DJointSample} simulates points in the plan
#' as if they were distributed uniformly on
#' a drawing of a frowning face.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param x the size of the sample
#' @param ... the size of the sample
#'
#' @return a list with two vectors of x and y of the samples coordinates.
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @export
#' @examples
#' print(frown(5000))
#'
#' @family Methods for 2dJointSample class
#'
#' @aliases print.jointSample1 print.jointSample2 print.jointSample3
print.2DJointSample <-
function(x, ...) {
  cat("Joint Sample\n")
  NextMethod("print", x)
  invisible(x)
}
#---- END print.2DJointSample -------------------------------

#---- BEGIN as.2DJointSample -------------------------------
#' Transform data.frame into an object of type jointSample
#'
#' \code{as.2DJointSample} takes a dataframe and extracts 2
#' columns of it and represent those as a two-dimensional
#' joint sample.
#'
#' This and the list of functions below allows the
#' user to quickly generate two-dimensional samples
#' with very different dependency structures.
#'
#' @param df a dataframe
#' @param dim1 the first column
#' @param dim2 the second column
#'
#' @return A list with two vectors of x and y of the samples
#'         coordinates. The two list members are also named
#'         x and y and the list itself is of class type
#'         \code{2dJointSample}.
#'
#' @export
# @examples
# as.2DJointSample(as.data.frame(frown(3000), y, x)
#'
#' @aliases as.2DJointSample1 as.2DJointSample2 as.2DJointSample3
as.2DJointSample <-
function(df, dim1, dim2) {
  me <- list(
             x = df[names(df)[dim1]],
             y = df[names(df)[dim2]]
             )
  class(me) <- "jointSample"
  me
}
#---- END as.2DJointSample ---------------------------------
