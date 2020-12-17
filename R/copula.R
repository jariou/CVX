#---- BEGIN copula -----------------------------------------
#' Generate the empirical copula from a joint random sample
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param joint_sample an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sampleas a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' copula(frown(1000))
#'
#' @aliases copula1 copula2 copula3
#----------------------------------------------
# Generate empirical copula from a joint sample
copula <-
function(joint_sample) {
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
#---- END copula -------------------------------------------

#---- BEGIN rand_copula -----------------------------------
#' Apply a different dependency structure to a joint
#' random sample.
#'
#' \code{rand_copula} generates a joint random sample
#' with the same marginals as the sample passed in as
#' and the empirical copula also passed in.
#'
#' @param cop an empirical copua object
#' @param sample an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return A jointsample with thesame copula as \code{cop} and
#'         the same marginals as \code{sample}
#'
#' @export
#' @examples
#' fr <- frown(1000)
#' sm <- smile(1000)
#' rand_copula(copula(fr), sm)
#'
#' @aliases rand_copula1 rand_copula2 rand_copula3
rand_copula <-
function(cop, sample) {
  size    <- length(cop$x)
  ss      <- lapply(sample, sort)
  s_cop_x <- ss$x[cop$x * (size + 1)]
  s_cop_y <- ss$x[cop$y * (size + 1)]

  me  <- list(
              x = s_cop_x,
              y = s_cop_y
              )
  class(me) <- append(class(me), "jointSample")
  return(me)
}
#---- END rand_copula -------------------------------------

#---- BEGIN Movie -----------------------------------------
#' Make a movie from going from one plot to another one
#' Generate the empirical copula from a joint random sample
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param start_point an object of type \code{2dJointSample}
#' @param end_point an object of type \code{2dJointSample}
#' @param steps an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sampleas a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' fr <- frown(1000)
#' Movie(fr, copula(fr), 100)
#'
#' @aliases Movie1 Movie2 Movie3
Movie <- function(start_point, end_point, steps) {
  x_start  <- start_point$x
  y_start  <- start_point$y
  x_end    <- end_point$x
  y_end    <- end_point$y
  x_diff   <- end_point$x - start_point$x
  y_diff   <- end_point$y - start_point$y


  for (i in 0:steps) {
    plot.2DJointSample(
                     list(
                          x = x_start + i / steps * x_diff,
                          y = y_start + i / steps * y_diff
                          )
                    )
  }

}
#---- END Movie -------------------------------------------

#---- BEGIN CopMovie --------------------------------------
# Make a movie from going from one plot to another one
#' Generate the empirical copula from a joint random sample
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param sample an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sampleas a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' CopMovie(frown(1000))
#'
#' @aliases CopMovie1 CopMovie2 CopMovie3
CopMovie <- function(sample) {
  Movie(sample, copula(sample), steps = 100)
}
#---- END CopMovie ---------------------------------------

#---- BEGIN One ---------------------------------------
# Perfect positive Dependency Copula
# Make a movie from going from one plot to another one
#' Generate the empirical copula from a joint random sample
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param n an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sampleas a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' One(100)
#'
#' @aliases One1 One2 One3
One <-
function(n) {
  me <- list(
             x = (1:n) / (n + 1),
             y = (1:n) / (n + 1)
            )
  class(me) <- append(class(me), "jointSample")
  class(me) <- append(class(me), "copula")
  return(me)
}
#---- END One ---------------------------------------

#---- BEGIN MinusOne ---------------------------------------
# Perfect negative Dependency Copula
# Perfect positive Dependency Copula
# Make a movie from going from one plot to another one
#' Generate the empirical copula from a joint random sample
#'
#' \code{copula} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param n an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sampleas a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' MinusOne(100)
#'
#' @aliases MinusOne1 MinusOne2 MinusOne3
MinusOne <-
function(n) {
  me <- list(
             x = (1:n) / (n + 1),
             y = (n:1) / (n + 1)
            )
  class(me) <- append(class(me), "jointSample")
  class(me) <- append(class(me), "copula")
  return(me)
}
#---- END MinusOne ---------------------------------------

#---- BEGIN common_scale ---------------------------------
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
#' @param n an object of type \code{2dJointSample}
#' @param scale_model an object of type \code{2dJointSample}
#' @param margin_models an object of type \code{2dJointSample}
#'
#' @return
#'         The two list members are also named x and y and the list
#'         itself is of class type \code{2dJointSample}.
#'
#' @return The empirical copula model of the joint sampleas a list
#'         with two vectors of x and y of the samples coordinates.
#'
#' @export
#' @examples
#' common_scale(frown(1000))
#'
#' @aliases common_scale1 common_scale2 common_scale3
common_scale <-
function(
         n,
         scale_model   = Exponential(100),
         margin_models = list(
                              x = Exponential(100),
                              y = Exponential(100)
                              )
         ) {
  scales <- scale_model$Roll(n)
  tmp <- lapply(margin_models, "[[", "Roll")
  me <- list(
             x = tmp$x(n) * scales,
             y = tmp$y(n) * scales
             )
  class(me) <- append(class(me), "jointSample")
  return(me)
}
#---- END common_scale ------------------------------------
