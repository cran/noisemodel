###############################################################
###############################################################
###############################################################

#' @export
sym_int_an <- function(x, ...) UseMethod("sym_int_an")

#' Symmetric interval-based attribute noise
#'
#' Introduction of \emph{Symmetric interval-based attribute noise} into a classification dataset.
#'
#' \emph{Symmetric interval-based attribute noise} corrupts (\code{level}·100)\% of the values of 
#' each attribute in the dataset. In order to corrupt an attribute \emph{A}, (\code{level}·100)\% of the
#' samples in the dataset are selected. To corrupt numeric
#' attributes, the attribute is split into \code{nbins} equal-frequency intervals, one of its closest
#' intervals is chosen and a random value within the interval
#' is picked out as noisy. For nominal attributes, a random value within the domain is chosen.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param nbins an integer with the number of bins to create (default: 10).
#' @param sortid a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).
#' @param formula a formula with the output class and, at least, one input attribute.
#' @param data a data frame in which to interpret the variables in the formula.
#' @param ... other options to pass to the function.
#'
#' @return An object of class \code{ndmodel} with elements:
#' \item{xnoise}{a data frame with the noisy input attributes.}
#' \item{ynoise}{a factor vector with the noisy output class.}
#' \item{numnoise}{an integer vector with the amount of noisy samples per attribute.}
#' \item{idnoise}{an integer vector list with the indices of noisy samples per attribute.}
#' \item{numclean}{an integer vector with the amount of clean samples per attribute.}
#' \item{idclean}{an integer vector list with the indices of clean samples per attribute.}
#' \item{distr}{an integer vector with the samples per class in the original data.}
#' \item{model}{the full name of the noise introduction model used.}
#' \item{param}{a list of the argument values.}
#' \item{call}{the function call.}
#'
#' @references
#' M. V. Mannino, Y. Yang, and Y. Ryu. 
#' \strong{Classification algorithm sensitivity to training data with non representative attribute noise}. 
#' \emph{Decision Support Systems}, 46(3):743-751, 2009.
#' \doi{10.1016/j.dss.2008.11.021}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_int_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_int_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{symd_uni_an}}, \code{\link{sym_uni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_int_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_int_an
#' @importFrom "stats" "runif"
#' @importFrom "classInt" "classIntervals"
sym_int_an.default <- function(x, y, level, nbins = 10, sortid = TRUE, ...){

  ######################################################
  # check for errors #########
  if(!is.data.frame(x)){
    stop("argument \"x\" must be a data frame")
  }
  if(!is.factor(y)){
    stop("argument \"y\" must be a factor vector")
  }
  if(nlevels(y) < 2){
    stop("argument \"y\" must have at least 2 levels")
  }
  if(level < 0 || level > 1){
    stop("argument \"level\" must be in [0,1]")
  }
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }
  if(nbins <= 1){
    stop("argument \"nbins\" must be higher than 1")
  }

  ######################################################
  # introduce noise #########
  num_noise <- round(level*nrow(x))
  idx_noise <- list()
  idx_clean <- list()

  if(num_noise > 0){
    for(a in 1:ncol(x)){
      idx_noise[[a]] <- sample(1:nrow(x), num_noise, replace = FALSE)
      if(sortid){
        idx_noise[[a]] <- sort(idx_noise[[a]])
      }
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])

      if(is.numeric(x[,a])){

        disc <- classIntervals(x[,a], nbins, style = 'quantile')
        breaks_ <- unique(disc$brks)
        realbins_ <- length(breaks_)-1

        min.int <- breaks_[1:(length(breaks_)-1)]
        max.int <- breaks_[2:length(breaks_)]

        part_j <- cut(x[,a], breaks = breaks_, labels = FALSE, include.lowest = TRUE, right = FALSE)

        for(sindx in 1:length(idx_noise[[a]])){
          s <- idx_noise[[a]][sindx]

          if(part_j[s] == 1)
            noiseint <- 2
          else if(part_j[s] == realbins_)
            noiseint <- realbins_ - 1
          else
            noiseint <- sample(x = c(part_j[s]-1, part_j[s]+1), size = 1)

          x[s,a] <- runif(n = 1, min = min.int[noiseint], max = max.int[noiseint])
        }

        if(is.integer(x[,a]))
          x[s,a] <- round(x[s,a])
      }

      else if(is.factor(x[,a])){
        newvalues <- sample_replace(x = 1:nlevels(x[,a]), size = num_noise, original = FALSE, ref = as.integer(x[idx_noise[[a]],a]))
        newvalues <- levels(x[,a])[newvalues]
        x[idx_noise[[a]],a] <- newvalues
      }

    }
  }
  else{
    for(a in 1:ncol(x)){
      idx_noise[[a]] <- integer(0)
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
    }
  }

  classes <- levels(y)
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_int_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = rep(num_noise,ncol(x)),
              idnoise = idx_noise,
              numclean = rep(nrow(x),ncol(x))-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Symmetric interval-based attribute noise",
              param = list(level = level, nbins = nbins, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_int_an
#' @importFrom "stats" "model.frame"
sym_int_an.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_int_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_int_an")

  return(res)
}

###############################################################
###############################################################
###############################################################
