###############################################################
###############################################################
###############################################################

#' @export
symd_uni_an <- function(x, ...) UseMethod("symd_uni_an")

#' Symmetric/dependent uniform attribute noise
#'
#' Introduction of \emph{Symmetric/dependent uniform attribute noise} into a classification dataset.
#'
#' \emph{Symmetric/dependent uniform attribute noise} corrupts (\code{level}·100)\% of the samples 
#' in the dataset.  
#' Their attribute values are replaced by random different ones between
#' the minimum and maximum of the domain of each attribute following a uniform distribution (for numerical
#' attributes) or choosing a random value (for nominal attributes).
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
#' A. Petety, S. Tripathi, and N. Hemachandra. 
#' \strong{Attribute noise robust binary classification}. 
#' In \emph{Proc. 34th AAAI Conference on Artificial Intelligence}, pages 13897-13898, 2020.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- symd_uni_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- symd_uni_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_uni_an}}, \code{\link{sym_cuni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name symd_uni_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname symd_uni_an
#' @importFrom "stats" "runif"
symd_uni_an.default <- function(x, y, level, sortid = TRUE, ...){
  
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
  num_noise <- round(level*nrow(x))
  idx_noise <- list()
  idx_clean <- list()
  
  sam <- sample(1:nrow(x), num_noise, replace = FALSE)
  
  if(num_noise > 0){
    for(a in 1:ncol(x)){
      
      idx_noise[[a]] <- sam
      if(sortid){
        idx_noise[[a]] <- sort(idx_noise[[a]])
      }
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
      
      if(is.integer(x[,a])){
        newvalues <- sample_replace(x = min(x[,a]):max(x[,a]), size = num_noise, original = FALSE, ref = x[idx_noise[[a]],a])
      }
      else if(is.double(x[,a])){
        newvalues <- runif_replace(n = num_noise, min = min(x[,a]), max = max(x[,a]), original = FALSE, ref = x[idx_noise[[a]],a])
      }
      else if(is.factor(x[,a])){
        newvalues <- sample_replace(x = 1:nlevels(x[,a]), size = num_noise, original = FALSE, ref = as.integer(x[idx_noise[[a]],a]))
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
  call[[1]] <- as.name("symd_uni_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = rep(num_noise,ncol(x)),
              idnoise = idx_noise,
              numclean = rep(nrow(x),ncol(x))-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Symmetric/dependent uniform attribute noise",
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
#' @rdname symd_uni_an
#' @importFrom "stats" "model.frame"
symd_uni_an.formula <- function(formula, data, ...){
  
  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }
  
  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL
  
  x <- mf[,-1]
  y <- mf[,1]
  
  res <- symd_uni_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("symd_uni_an")
  
  return(res)
}

###############################################################
###############################################################
###############################################################
