###############################################################
###############################################################
###############################################################

#' @export
unc_vgau_an <- function(x, ...) UseMethod("unc_vgau_an")

#' Unconditional vp-Gaussian attribute noise
#'
#' Introduction of \emph{Unconditional vp-Gaussian attribute noise} into a classification dataset.
#'
#' In \emph{Unconditional vp-Gaussian attribute noise}, the noise level for numeric attributes indicates 
#' the magnitude of the errors introduced. For each attribute \emph{A}, all the original values are corrupted 
#' by adding a random number that follows a Gaussian distribution with \emph{mean} = 0 and 
#' \emph{variance} = \code{level}\%
#' of the variance of \emph{A}. For nominal attributes, (\code{level}·100)\% of the samples in the dataset 
#' are chosen and a random value is selected as noisy.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
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
#' X. Huang, L. Shi, and J. A. K. Suykens. 
#' \strong{Support vector machine classifier with pinball loss}. 
#' \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence}, 36(5):984-997, 2014.
#' \doi{10.1109/TPAMI.2013.178}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- unc_vgau_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- unc_vgau_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References, corrupting all samples and 
#' allowing nominal attributes.
#'
#' @seealso \code{\link{symd_rpix_an}}, \code{\link{unc_fixw_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name unc_vgau_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname unc_vgau_an
#' @importFrom "stats" "rnorm" "sd"
unc_vgau_an.default <- function(x, y, level, sortid = TRUE, ...){

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

  ######################################################
  # introduce noise #########
  num_noise <- rep(NA, ncol(x))
  for(a in 1:ncol(x)){
    if(is.numeric(x[,a]))
      num_noise[a] <- nrow(x)
    else
      num_noise[a] <- round(level*nrow(x))
  }
  idx_noise <- list()
  idx_clean <- list()

  if(level > 0){
    for(a in 1:ncol(x)){

      idx_noise[[a]] <- sample(1:nrow(x), num_noise[a], replace = FALSE)
      if(sortid){
        idx_noise[[a]] <- sort(idx_noise[[a]])
      }
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])

      if(is.numeric(x[,a])){
        newvalues <- x[idx_noise[[a]],a] + rnorm(n = num_noise[a], mean = 0, sd = level*sd(x[,a]))
        if(is.integer(x[,a])){
          newvalues <- round(newvalues)
        }
        newvalues[newvalues < min(x[,a])] <- min(x[,a])
        newvalues[newvalues > max(x[,a])] <- max(x[,a])
      }
      else if(is.factor(x[,a])){
        newvalues <- sample_replace(x = 1:nlevels(x[,a]), size = num_noise[a], original = FALSE, ref = as.integer(x[idx_noise[[a]],a]))
        newvalues <- levels(x[,a])[newvalues]
      }

      x[idx_noise[[a]],a] <- newvalues
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
  call[[1]] <- as.name("unc_vgau_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = num_noise,
              idnoise = idx_noise,
              numclean = nrow(x)-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Unconditional vp-Gaussian attribute noise",
              param = list(level = level, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname unc_vgau_an
#' @importFrom "stats" "model.frame"
unc_vgau_an.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- unc_vgau_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("unc_vgau_an")

  return(res)
}

###############################################################
###############################################################
###############################################################
