#---- BEGIN my_cdf ----------------------------------------
# Simple implementation of empirical cdf
# Common Scale Factor Dependent Model
# Perfect negative Dependency Copula
# Perfect positive Dependency Copula
# Make a movie from going from one plot to another one
#' Generate the empirical copula from a joint random sample
#'
#' \code{my_cdf} generates the empirical copula
#' corresponding to the joint sample passed in
#' as input
#'
#' @param x an object of type \code{2DSample}
#'
#' @return The empirical copula model of the joint sample
#'         as a list with two vectors of x and y of the
#'         samples coordinates.
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
       x = sx,
       y = my_y
       )

  # list(
  #      x = my_x,
  #      y = my_y
  #      )

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
#'         itself is of class type \code{2DSample}.
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
#'         itself is of class type \code{2DSample}.
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
#'         itself is of class type \code{2DSample}.
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
#'         itself is of class type \code{2DSample}.
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
#'         itself is of class type \code{2DSample}.
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


# Outline
# Section 14.2 introduces 
# 
# R6::R6class(), the one function that you need to know to create 
# R6 classes. You’ll learn about the constructor method, 
# 
# $new(), which allows you to create R6 objects, as well as other 
# important methods like 
# 
# $initialize() and 
# $print().
# 
# Section 14.3 discusses the access mechanisms of R6: private and 
# active fields. Together, these allow you to hide data from the 
# user, or expose private data for reading but not writing.
# 
# Section 14.4 explores the consequences of R6’s reference semantics. 
# You’ll learn about the use of finalizers to automatically clean up 
# any operations performed in the initializer, and a common gotcha if 
# you use an R6 object as a field in another R6 object.
# 
# Section 14.5 describes why I cover R6, rather than the base RC system.
# 
# Prerequisites
# Because R6 is not built into base R, you’ll need to install and load 
# the R6 package to use it:
# 
# install.packages("R6")
# library(R6)
# 
# R6 objects have reference semantics which means that they are modified 
# in-place, not copied-on-modify. If you’re not familiar with these terms, 
# brush up your vocab by reading Section 2.5.
# 
# 14.2 Classes and methods
# 
# R6 only needs a single function call to create both the class and its 
# methods: 
# 
# R6::R6Class(). 
# 
# This is the only function from the package that you’ll ever use!74
# 
# The following example shows the two most important arguments to R6Class():
# 
# The first argument is the classname. 
# 
# It’s not strictly needed, but it improves error messages and makes it 
# possible to use R6 objects with S3 generics. By convention, R6 classes 
# have UpperCamelCase names.
# 
# The second argument, 
# 
# public, 
# 
# supplies a list of methods (functions) and fields (anything else) that 
# make up the public interface of the object. By convention, methods and 
# fields use snake_case. Methods can access the methods and fields of the 
# current object via self$.75
# 
# Accumulator <- R6Class("Accumulator", list(
#   sum = 0,
#   add = function(x = 1) {
#     self$sum <- self$sum + x 
#     invisible(self)
#   })
# )
# 
# You should always assign the result of R6Class() into a variable with the 
# same name as the class, because R6Class() returns an R6 object that defines 
# the class:
# 
# Accumulator
#> <Accumulator> object generator
#>   Public:
#>     sum: 0
#>     add: function (x = 1) 
#>     clone: function (deep = FALSE) 
#>   Parent env: <environment: R_GlobalEnv>
#>   Locked objects: TRUE
#>   Locked class: FALSE
#>   Portable: TRUE
# 
# You construct a new object from the class by calling the new() method. In R6,
# methods belong to objects, so you use $ to access new():
# 
# x <- Accumulator$new() 
# 
# You can then call methods and access fields with $:
# 
# x$add(4) 
# x$sum
#> [1] 4
# 
# In this class, the fields and methods are public, which means that you can get 
# or set the value of any field. Later, we’ll see how to use private fields and 
# methods to prevent casual access to the internals of your class.
# 
# To make it clear when we’re talking about fields and methods as opposed to 
# variables and functions, I’ll prefix their names with $. For example, the 
# Accumulate class has field $sum and method $add().
# 
# 14.2.1 Method chaining
# 
# $add() is called primarily for its side-effect of updating $sum.
# 
# Accumulator <- R6Class("Accumulator", list(
#   sum = 0,
#   add = function(x = 1) {
#     self$sum <- self$sum + x 
#     invisible(self)
#   })
# )
# 
# Side-effect R6 methods should always return self invisibly. This returns the 
# “current” object and makes it possible to chain together multiple method calls:
# 
# x$add(10)$add(10)$sum
#> [1] 24
# 
# For, readability, you might put one method call on each line:
# 
# x$
#   add(10)$
#   add(10)$
#   sum
#> [1] 44
# 
# This technique is called method chaining and is commonly used in languages like 
# Python and JavaScript. Method chaining is deeply related to the pipe, and we’ll 
# discuss the pros and cons of each approach in Section 16.3.3.
# 
# 14.2.2 Important methods
# 
# There are two important methods that should be defined for most classes: 
# 
# $initialize() 
# 
# and 
# 
# $print(). 
# 
# They’re not required, but providing them will make your class easier to use.
# 
# $initialize() overrides the default behaviour of $new(). For example, the 
# following code defines an Person class with fields $name and $age. To ensure 
# that that $name is always a single string, and $age is always a single number, 
# I placed checks in $initialize().
# 
# Person <- R6Class("Person", list(
#   name = NULL,
#   age = NA,
#   initialize = function(name, age = NA) {
#     stopifnot(is.character(name), length(name) == 1)
#     stopifnot(is.numeric(age), length(age) == 1)
#     
#     self$name <- name
#     self$age <- age
#   }
# ))
# 
# hadley <- Person$new("Hadley", age = "thirty-eight")
#> Error in initialize(...): is.numeric(age) is not TRUE
# 
# hadley <- Person$new("Hadley", age = 38)
# 
# If you have more expensive validation requirements, implement them in a separate 
# 
# $validate() 
# 
# and only call when needed.
# 
# Defining 
# 
# $print() 
# 
# allows you to override the default printing behaviour. As with any R6 method called 
# for its side effects, $print() should return invisible(self).
# 
# Person <- R6Class("Person", list(
#   name = NULL,
#   age = NA,
#   initialize = function(name, age = NA) {
#     self$name <- name
#     self$age <- age
#   },
#   print = function(...) {
#     cat("Person: \n")
#     cat("  Name: ", self$name, "\n", sep = "")
#     cat("  Age:  ", self$age, "\n", sep = "")
#     invisible(self)
#   }
# ))
# 
# hadley2 <- Person$new("Hadley")
# hadley2
#> Person: 
#>   Name: Hadley
#>   Age:  NA
# This code illustrates an important aspect of R6. Because methods are bound 
# to individual objects, the previously created hadley object does not get 
# this new method:
# 
# hadley
#> <Person>
#>   Public:
#>     age: 38
#>     clone: function (deep = FALSE) 
#>     initialize: function (name, age = NA) 
#>     name: Hadley
# 
# hadley$print
#> NULL
# 
# From the perspective of R6, there is no relationship between hadley and hadley2; 
# they just coincidentally share the same class name. This doesn’t cause problems 
# when using already developed R6 objects but can make interactive experimentation 
# confusing. If you’re changing the code and can’t figure out why the results of 
# method calls aren’t any different, make sure you’ve re-constructed R6 objects 
# with the new class.
# 
# 14.2.3 Adding methods after creation
# 
# Instead of continuously creating new classes, it’s also possible to modify 
# the fields and methods of an existing class. This is useful when exploring 
# interactively, or when you have a class with many functions that you’d like 
# to break up into pieces. Add new elements to an existing class with $set(), 
# supplying the visibility (more on in Section 14.3), the name, and the 
# component.
# 
# Accumulator <- R6Class("Accumulator")
# Accumulator$set("public", "sum", 0)
# Accumulator$set("public", "add", function(x = 1) {
#   self$sum <- self$sum + x 
#   invisible(self)
# })
# 
# As above, new methods and fields are only available to new objects; they are 
# not retrospectively added to existing objects.
# 
# 14.2.4 Inheritance
# 
# To inherit behaviour from an existing class, provide the class object to the 
# inherit argument:
# 
# AccumulatorChatty <- R6Class("AccumulatorChatty", 
#   inherit = Accumulator,
#   public = list(
#     add = function(x = 1) {
#       cat("Adding ", x, "\n", sep = "")
#       super$add(x = x)
#     }
#   )
# )
# 
# x2 <- AccumulatorChatty$new()
# x2$add(10)$add(1)$sum
#> Adding 10
#> Adding 1
#> [1] 11
# 
# $add() overrides the superclass implementation, but we can still delegate 
# to the superclass implementation by using super$. (This is analogous to 
# NextMethod() in S3, as discussed in Section 13.6.) Any methods which are 
# not overridden will use the implementation in the parent class.
# 
# 14.2.5 Introspection
# 
# Every R6 object has an S3 class that reflects its hierarchy of R6 classes. 
# This means that the easiest way to determine the class (and all classes it 
# inherits from) is to use class():
# 
# class(hadley2)
#> [1] "Person" "R6"
# 
# The S3 hierarchy includes the base “R6” class. This provides common behaviour, 
# including a print.R6() method which calls $print(), as described above.
# 
# You can list all methods and fields with names():
# 
# names(hadley2)
#> [1] ".__enclos_env__" "age"             "name"            "clone"          
#> [5] "print"           "initialize"
# 
# We defined $name, $age, $print, and $initialize. As suggested by the name, 
# .__enclos_env__ is an internal implementation detail that you shouldn’t 
# touch; we’ll come back to $clone() in Section 14.4.
# 
# 14.2.6 Exercises
# 
# Create a bank account R6 class that stores a balance and allows you to 
# deposit and withdraw money. Create a subclass that throws an error if 
# you attempt to go into overdraft. Create another subclass that allows 
# you to go into overdraft, but charges you a fee.
# 
# Create an R6 class that represents a shuffled deck of cards. You should 
# be able to draw cards from the deck with $draw(n), and return all cards 
# to the deck and reshuffle with $reshuffle(). Use the following code to 
# make a vector of cards.
# 
# suit <- c("♠", "♥", "♦", "♣")
# value <- c("A", 2:10, "J", "Q", "K")
# cards <- paste0(rep(value, 4), suit)
# 
# Why can’t you model a bank account or a deck of cards with an S3 class?
# 
# Create an R6 class that allows you to get and set the current timezone. 
# You can access the current timezone with Sys.timezone() and set it with 
# Sys.setenv(TZ = "newtimezone"). When setting the time zone, make sure 
# the new time zone is in the list provided by OlsonNames().
# 
# Create an R6 class that manages the current working directory. It should 
# have $get() and $set() methods.
# 
# Why can’t you model the time zone or current working directory with an 
# S3 class?
# 
# What base type are R6 objects built on top of? What attributes do they have?
# 
# 14.3 Controlling access
# 
# R6Class() has two other arguments that work similarly to public:
# 
# private allows you to create fields and methods that are only available 
# from within the class, not outside of it.
# 
# active allows you to use accessor functions to define dynamic, or active, 
# fields.
# 
# These are described in the following sections.
# 
# 14.3.1 Privacy
# 
# With R6 you can define private fields and methods, elements that can only 
# be accessed from within the class, not from the outside76. There are two 
# things that you need to know to take advantage of private elements:
# 
# The private argument to R6Class works in the same way as the public argument: 
# you give it a named list of methods (functions) and fields (everything else).
# 
# Fields and methods defined in private are available within the methods using 
# private$ instead of self$. You cannot access private fields or methods 
# outside of the class.
# 
# To make this concrete, we could make $age and $name fields of the Person 
# class private. With this definition of Person we can only set $age and 
# $name during object creation, and we cannot access their values from 
# outside of the class.
# 
# Person <- R6Class("Person", 
#   public = list(
#     initialize = function(name, age = NA) {
#       private$name <- name
#       private$age <- age
#     },
#     print = function(...) {
#       cat("Person: \n")
#       cat("  Name: ", private$name, "\n", sep = "")
#       cat("  Age:  ", private$age, "\n", sep = "")
#     }
#   ),
#   private = list(
#     age = NA,
#     name = NULL
#   )
# )
# 
# hadley3 <- Person$new("Hadley")
# hadley3
#> Person: 
#>   Name: Hadley
#>   Age:  NA
# hadley3$name
#> NULL
# 
# The distinction between public and private fields is important when you 
# create complex networks of classes, and you want to make it as clear as 
# possible what it’s ok for others to access. Anything that’s private can 
# be more easily refactored because you know others aren’t relying on it. 
# Private methods tend to be less important in R compared to other 
# programming languages because the object hierarchies in R tend to be 
# simpler.
# 
# 14.3.2 Active fields
# 
# Active fields allow you to define components that look like fields from 
# the outside, but are defined with functions, like methods. Active fields 
# are implemented using active bindings (Section 7.2.6). Each active 
# binding is a function that takes a single argument: value. If the 
# argument is missing(), the value is being retrieved; otherwise it’s being 
# modified.
# 
# For example, you could make an active field random that returns a different 
# value every time you access it:
# 
# Rando <- R6::R6Class("Rando", active = list(
#   random = function(value) {
#     if (missing(value)) {
#       runif(1)  
#     } else {
#       stop("Can't set `$random`", call. = FALSE)
#     }
#   }
# ))
# x <- Rando$new()
# x$random
#> [1] 0.0808
# x$random
#> [1] 0.834
# x$random
#> [1] 0.601
# 
# Active fields are particularly useful in conjunction with private fields, 
# because they make it possible to implement components that look like fields 
# from the outside but provide additional checks. For example, we can use 
# them to make a read-only age field, and to ensure that name is a length 1 
# character vector.
# # 
# Person <- R6Class("Person", 
#   private = list(
#     .age = NA,
#     .name = NULL
#   ),
#   active = list(
#     age = function(value) {
#       if (missing(value)) {
#         private$.age
#       } else {
#         stop("`$age` is read only", call. = FALSE)
#       }
#     },
#     name = function(value) {
#       if (missing(value)) {
#         private$.name
#       } else {
#         stopifnot(is.character(value), length(value) == 1)
#         private$.name <- value
#         self
#       }
#     }
#   ),
#   public = list(
#     initialize = function(name, age = NA) {
#       private$.name <- name
#       private$.age <- age
#     }
#   )
# )
# 
# hadley4 <- Person$new("Hadley", age = 38)
# hadley4$name
#> [1] "Hadley"
# hadley4$name <- 10
#> Error in (function (value) : is.character(value) is not TRUE
# hadley4$age <- 20
#> Error: `$age` is read only
# 
# 14.3.3 Exercises
# 
# Create a bank account class that prevents you from directly setting the 
# account balance, but you can still withdraw from and deposit to. Throw 
# an error if you attempt to go into overdraft.
# 
# Create a class with a write-only $password field. It should have 
# $check_password(password) method that returns TRUE or FALSE, but there 
# should be no way to view the complete password.
# 
# Extend the Rando class with another active binding that allows you to 
# access the previous random value. Ensure that active binding is the only 
# way to access the value.
# 
# Can subclasses access private fields/methods from their parent? Perform 
# an experiment to find out.
# 
# 14.4 Reference semantics
# 
# One of the big differences between R6 and most other objects is that 
# they have reference semantics. The primary consequence of reference 
# semantics is that objects are not copied when modified:
# 
# y1 <- Accumulator$new() 
# y2 <- y1
# 
# y1$add(10)
# c(y1 = y1$sum, y2 = y2$sum)
#> y1 y2 
#> 10 10
# 
# Instead, if you want a copy, you’ll need to explicitly $clone() the object:
# 
# y1 <- Accumulator$new() 
# y2 <- y1$clone()
# 
# y1$add(10)
# c(y1 = y1$sum, y2 = y2$sum)
#> y1 y2 
#> 10  0
# 
# ($clone() does not recursively clone nested R6 objects. If you want that, 
# you’ll need to use $clone(deep = TRUE).)
# 
# There are three other less obvious consequences:
# 
# It is harder to reason about code that uses R6 objects because you need 
# to understand more context.
# 
# It makes sense to think about when an R6 object is deleted, and you can 
# write a $finalize() to complement the $initialize().
# 
# If one of the fields is an R6 object, you must create it inside 
# $initialize(), not R6Class().
# 
# These consequences are described in more detail below.
# 
# 14.4.1 Reasoning
# 
# Generally, reference semantics makes code harder to reason about. Take this 
# very simple example:
# 
# x <- list(a = 1)
# y <- list(b = 2)
# 
# z <- f(x, y)
# For the vast majority of functions, you know that the final line only modifies z.
# 
# Take a similar example that uses an imaginary List reference class:
# 
# x <- List$new(a = 1)
# y <- List$new(b = 2)
# 
# z <- f(x, y)
# 
# The final line is much harder to reason about: if f() calls methods of x or y, 
# it might modify them as well as z. This is the biggest potential downside of 
# R6 and you should take care to avoid it by writing functions that either return 
# a value, or modify their R6 inputs, but not both. That said, doing both can lead 
# to substantially simpler code in some cases, and we’ll discuss this further in 
# Section 16.3.2.
# 
# 14.4.2 Finalizer
# 
# One useful property of reference semantics is that it makes sense to think about 
# when an R6 object is finalized, i.e. when it’s deleted. This doesn’t make sense 
# for most objects because copy-on-modify semantics mean that there may be many 
# transient versions of an object, as alluded to in Section 2.6. For example, the 
# following creates two factor objects: the second is created when the levels are 
# modified, leaving the first to be destroyed by the garbage collector.
# 
# x <- factor(c("a", "b", "c"))
# levels(x) <- c("c", "b", "a")
# 
# Since R6 objects are not copied-on-modify they are only deleted once, and it makes 
# sense to think about $finalize() as a complement to $initialize(). Finalizers 
# usually play a similar role to on.exit() (as described in Section 6.7.4), cleaning 
# up any resources created by the initializer. For example, the following class wraps 
# up a temporary file, automatically deleting it when the class is finalized.
# 
# TemporaryFile <- R6Class("TemporaryFile", list(
#   path = NULL,
#   initialize = function() {
#     self$path <- tempfile()
#   },
#   finalize = function() {
#     message("Cleaning up ", self$path)
#     unlink(self$path)
#   }
# ))
# 
# The finalize method will be run when the object is deleted (or more precisely, 
# by the first garbage collection after the object has been unbound from all names) 
# or when R exits. This means that the finalizer can be called effectively anywhere 
# in your R code, and therefore it’s almost impossible to reason about finalizer 
# code that touches shared data structures. Avoid these potential problems by only 
# using the finalizer to clean up private resources allocated by initializer.
# 
# tf <- TemporaryFile$new()
# rm(tf)
#> Cleaning up /tmp/Rtmpk73JdI/file155f31d8424bd
# 
# 14.4.3 R6 fields
# 
# A final consequence of reference semantics can crop up where you don’t expect it. 
# If you use an R6 class as the default value of a field, it will be shared across 
# all instances of the object! Take the following code: we want to create a 
# temporary database every time we call TemporaryDatabase$new(), but the current 
# code always uses the same path.
# 
# TemporaryDatabase <- R6Class("TemporaryDatabase", list(
#   con = NULL,
#   file = TemporaryFile$new(),
#   initialize = function() {
#     self$con <- DBI::dbConnect(RSQLite::SQLite(), path = file$path)
#   },
#   finalize = function() {
#     DBI::dbDisconnect(self$con)
#   }
# ))
# 
# db_a <- TemporaryDatabase$new()
# db_b <- TemporaryDatabase$new()
# 
# db_a$file$path == db_b$file$path
#> [1] TRUE
# 
# (If you’re familiar with Python, this is very similar to the “mutable default 
# argument” problem.)
# 
# The problem arises because TemporaryFile$new() is called only once when the 
# TemporaryDatabase class is defined. To fix the problem, we need to make sure 
# it’s called every time that TemporaryDatabase$new() is called, i.e. we need 
# to put it in $initialize():
# 
# TemporaryDatabase <- R6Class("TemporaryDatabase", list(
#   con = NULL,
#   file = NULL,
#   initialize = function() {
#     self$file <- TemporaryFile$new()
#     self$con <- DBI::dbConnect(RSQLite::SQLite(), path = file$path)
#   },
#   finalize = function() {
#     DBI::dbDisconnect(self$con)
#   }
# ))
# 
# db_a <- TemporaryDatabase$new()
# db_b <- TemporaryDatabase$new()
# 
# db_a$file$path == db_b$file$path
#> [1] FALSE
# 
# 14.4.4 Exercises
# 
# Create a class that allows you to write a line to a specified file. You should 
# open a connection to the file in $initialize(), append a line using cat() in 
# $append_line(), and close the connection in $finalize().
# 
# 
# 14.5 Why R6?
# 
# R6 is very similar to a built-in OO system called reference classes, or RC for 
# short. I prefer R6 to RC because:
# 
# R6 is much simpler. Both R6 and RC are built on top of environments, but while 
# R6 uses S3, RC uses S4. This means to fully understand RC, you need to 
# understand how the more complicated S4 works.
# 
# R6 has comprehensive online documentation at https://r6.r-lib.org.
# 
# R6 has a simpler mechanism for cross-package subclassing, which just works 
# without you having to think about it. For RC, read the details in the 
# “External Methods; Inter-Package Superclasses” section of ?setRefClass.
# 
# RC mingles variables and fields in the same stack of environments so that 
# you get (field) and set (field <<- value) fields like regular values. R6 
# puts fields in a separate environment so you get (self$field) and set 
# (self$field <- value) with a prefix. The R6 approach is more verbose but 
# I like it because it is more explicit.
# 
# R6 is much faster than RC. Generally, the speed of method dispatch is not 
# important outside of microbenchmarks. However, RC is quite slow, and 
# switching from RC to R6 led to a substantial performance improvement in 
# the shiny package. For more details, see vignette("Performance", "R6").
# 
# RC is tied to R. That means if any bugs are fixed, you can only take 
# advantage of the fixes by requiring a newer version of R. This makes 
# it difficult for packages (like those in the tidyverse) that need to 
# work across many R versions.
# 
# Finally, because the ideas that underlie R6 and RC are similar, it will 
# only require a small amount of additional effort to learn RC if you need 
# to.
# 
# 
# 
# 16.3 R6 versus S3
# 
# R6 is a profoundly different OO system from S3 and S4 because it is built 
# on encapsulated objects, rather than generic functions. Additionally R6 
# objects have reference semantics, which means that they can be modified 
# in place. These two big differences have a number of non-obvious 
# consequences which we’ll explore here:
# 
# A generic is a regular function so it lives in the global namespace. 
# An R6 method belongs to an object so it lives in a local namespace. 
# This influences how we think about naming.
# 
# R6’s reference semantics allow methods to simultaneously return a
# value and modify an object. This solves a painful problem called 
# “threading state”.
# 
# You invoke an R6 method using $, which is an infix operator. If you 
# set up your methods correctly you can use chains of method calls as 
# an alternative to the pipe.
# 
# These are general trade-offs between functional and encapsulated OOP, 
# so they also serve as a discussion of system design in R versus 
# Python.
# 
# 16.3.1 Namespacing
# 
# One non-obvious difference between S3 and R6 is the space in which 
# methods are found:
# 
# Generic functions are global: all packages share the same namespace.
# 
# Encapsulated methods are local: methods are bound to a single object.
# 
# The advantage of a global namespace is that multiple packages can use 
# the same verbs for working with different types of objects. Generic 
# functions provide a uniform API that makes it easier to perform typical 
# actions with a new object because there are strong naming conventions. 
# This works well for data analysis because you often want to do the same 
# thing to different types of objects. In particular, this is one reason 
# that R’s modelling system is so useful: regardless of where the model 
# has been implemented you always work with it using the same set of 
# tools (summary(), predict(), …).
# 
# The disadvantage of a global namespace is that it forces you to think 
# more deeply about naming. You want to avoid multiple generics with the 
# same name in different packages because it requires the user to type 
# :: frequently. This can be hard because function names are usually 
# English verbs, and verbs often have multiple meanings. Take plot() 
# for example:
# 
# plot(data)       # plot some data
# plot(bank_heist) # plot a crime
# plot(land)       # create a new plot of land
# plot(movie)      # extract plot of a movie
# 
# Generally, you should avoid methods that are homonyms of the original 
# generic, and instead define a new generic.
# 
# This problem doesn’t occur with R6 methods because they are scoped to 
# the object. The following code is fine, because there is no implication 
# that the plot method of two different R6 objects has the same meaning:
# 
# data$plot()
# bank_heist$plot()
# land$plot()
# movie$plot()
# 
# These considerations also apply to the arguments to the generic. S3 
# generics must have the same core arguments, which means they generally 
# have non-specific names like x or .data. S3 generics generally need 
# ... to pass on additional arguments to methods, but this has the 
# downside that misspelled argument names will not create an error. In 
# comparison, R6 methods can vary more widely and use more specific and 
# evocative argument names.
# 
# A secondary advantage of local namespacing is that creating an R6 
# method is very cheap. Most encapsulated OO languages encourage you 
# to create many small methods, each doing one thing well with an 
# evocative name. Creating a new S3 method is more expensive, because 
# you may also have to create a generic, and think about the naming 
# issues described above. That means that the advice to create many 
# small methods does not apply to S3. It’s still a good idea to break 
# your code down into small, easily understood chunks, but they should 
# generally just be regular functions, not methods.
# 
# 16.3.2 Threading state
# 
# One challenge of programming with S3 is when you want to both return 
# a value and modify the object. This violates our guideline that a 
# function should either be called for its return value or for its side 
# effects, but is necessary in a handful of cases.
# 
# For example, imagine you want to create a stack of objects. A stack 
# has two main methods:
# 
# push() adds a new object to the top of the stack.
# pop() returns the top most value, and removes it from the stack.
# 
# The implementation of the constructor and the push() method is 
# straightforward. A stack contains a list of items, and pushing 
# an object to the stack simply appends to this list.
# 
# new_stack <- function(items = list()) {
#   structure(list(items = items), class = "stack")
# }
# 
# push <- function(x, y) {
#   x$items <- c(x$items, list(y))
#   x
# }
# 
# (I haven’t created a real method for push() because making it generic 
# would just make this example more complicated for no real benefit.)
# 
# Implementing pop() is more challenging because it has to both return 
# a value (the object at the top of the stack), and have a side-effect 
# (remove that object from that top). Since we can’t modify the input 
# object in S3 we need to return two things: the value, and the 
# updated object.
# 
# pop <- function(x) {
#   n <- length(x$items)
#   
#   item <- x$items[[n]]
#   x$items <- x$items[-n]
#   
#   list(item = item, x = x)
# }
# This leads to rather awkward usage:
# 
# s <- new_stack()
# s <- push(s, 10)
# s <- push(s, 20)
# 
# out <- pop(s)
# out$item
#> [1] 20
# s <- out$x
# s
#> $items
#> $items[[1]]
#> [1] 10
#> 
#> 
#> attr(,"class")
#> [1] "stack"
# 
# This problem is known as threading state or accumulator programming, 
# because no matter how deeply the pop() is called, you have to thread 
# the modified stack object all the way back to where it lives.
# 
# One way that other FP languages deal with this challenge is to provide 
# a multiple assign (or destructuring bind) operator that allows you to 
# assign multiple values in a single step. The zeallot package83 provides 
# multi-assign for R with %<-%. This makes the code more elegant, but 
# doesn’t solve the key problem:
# 
# library(zeallot)
# 
# c(value, s) %<-% pop(s)
# value
#> [1] 10
# 
# An R6 implementation of a stack is simpler because $pop() can modify 
# the object in place, and return only the top-most value:
# 
# Stack <- R6::R6Class("Stack", list(
#   items = list(),
#   push = function(x) {
#     self$items <- c(self$items, x)
#     invisible(self)
#   },
#   pop = function() {
#     item <- self$items[[self$length()]]
#     self$items <- self$items[-self$length()]
#     item
#   },
#   length = function() {
#     length(self$items)
#   }
# ))
# 
# This leads to more natural code:
# 
# s <- Stack$new()
# s$push(10)
# s$push(20)
# s$pop()
#> [1] 20
# 
# I encountered a real-life example of threading state in ggplot2 scales. 
# Scales are complex because they need to combine data across every facet 
# and every layer. I originally used S3 classes, but it required passing 
# scale data to and from many functions. Switching to R6 made the code 
# substantially simpler. However, it also introduced some problems because 
# I forgot to call to $clone() when modifying a plot. This allowed 
# independent plots to share the same scale data, creating a subtle bug 
# that was hard to track down.
# 
# 16.3.3 Method chaining
# 
# The pipe, %>%, is useful because it provides an infix operator that 
# makes it easy to compose functions from left-to-right. Interestingly, 
# the pipe is not so important for R6 objects because they already use 
# an infix operator: $. This allows the user to chain together multiple 
# method calls in a single expression, a technique known as method 
# chaining:
# 
# s <- Stack$new()
# s$
#   push(10)$
#   push(20)$
#   pop()
#> [1] 20
# 
# This technique is commonly used in other programming languages, like 
# Python and JavaScript, and is made possible with one convention: any 
# R6 method that is primarily called for its side-effects (usually 
# modifying the object) should return invisible(self).
# 
# The primary advantage of method chaining is that you can get useful 
# autocomplete; the primary disadvantage is that only the creator of 
# the class can add new methods (and there’s no way to use multiple 
# dispatch)