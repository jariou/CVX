# You can use roxygen to provide a help page for your package as
# a whole. This is accessed with package?foo, and can be used to
# describe the most important components of your package. It’s a
# useful supplement to vignettes, as described in the next chapter.
#
# There’s no object that corresponds to a package, so you need to
# document NULL, and then manually label it with @docType package
# and @name <package-name>. This is also an excellent place to use
# the @section tag to divide up page into useful categories.
#
# #' foo: A package for computating the notorious bar statistic
# #'
# #' The foo package provides three categories of important functions:
# #' foo, bar and baz.
# #'
# #' @section Foo functions:
# #' The foo functions ...
# #'
# #' @docType package
# #' @name foo
# NULL
# #> NULL
#
# I usually put this documentation in a file called <package-name>.R.
# It’s also a good place to put the package level import statements
# that you’ll learn about in imports.


#' CVX: A package for computating the notorious bar statistic
#'
#' The CVX package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Simple two dimensional joint sample models:
#'
#' - \code{\link{frown}}
#' - \link{smile}
#' - \link{circle}
#' - \link{pizza}
#' - \link{square}
#' - \link{triangle}
#'
#' @section Copula models:
#'
#'  \code{CommonShock}
#' \code{ErlangMixtures}
#' 
#' @section Utility functions:
#' 
#' The foo functions ...
#'
#' @section Other functions:
#' The foo functions ...
#'
#' @docType package
#' @name CVX
NULL
#> NULL


#---- END pizza -------------------------------------------
#---- END square ------------------------------------------
#---- END triangle ----------------------------------------
