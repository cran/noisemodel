###############################################################
###############################################################
###############################################################

#' @export
unc_fixw_an <- function(x, ...) UseMethod("unc_fixw_an")

#' Unconditional fixed-width attribute noise
#'
#' Introduction of \emph{Unconditional fixed-width attribute noise} into a classification dataset.
#'
#' \emph{Unconditional fixed-width attribute noise} corrupts all the samples in the dataset. 
#' For each attribute \emph{A}, all the original values are corrupted by adding a random number in the interval 
#' [-\emph{width}, \emph{width}], being \emph{width} = (\emph{max}(\emph{A})-\emph{min}(\emph{A}))·k. For 
#' nominal attributes, (\code{level}·100)\% of the samples in the dataset 
#' are chosen and a random value is selected as noisy.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced in nominal attributes.
#' @param k a double in [0,1] with the domain proportion of the noise width (default: 0.1).
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
#' A. Ramdas, B. Poczos, A. Singh, and L. A. Wasserman. 
#' \strong{An analysis of active learning with uniform feature noise}. 
#' In \emph{Proc. 17th International Conference on Artificial Intelligence and Statistics}, 
#' volume 33 of JMLR, pages 805-813, 2014.
#' url:\url{http://proceedings.mlr.press/v33/ramdas14.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- unc_fixw_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- unc_fixw_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References, corrupting all samples and 
#' allowing nominal attributes.
#'
#' @seealso \code{\link{sym_end_an}}, \code{\link{sym_sgau_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name unc_fixw_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname unc_fixw_an
unc_fixw_an.default <- function(x, y, level, k = 0.1, sortid = TRUE, ...){

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
  if(k < 0 || k > 1){
    stop("argument \"k\" must be in [0,1]")
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
        width <- (max(x[,a])-min(x[,a]))*k
        newvalues <- x[idx_noise[[a]],a] + runif(n = num_noise[a], min = -width, max = width)
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
  call[[1]] <- as.name("unc_fixw_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = num_noise,
              idnoise = idx_noise,
              numclean = nrow(x)-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Unconditional fixed-width attribute noise",
              param = list(level = level, k = k, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname unc_fixw_an
#' @importFrom "stats" "model.frame"
unc_fixw_an.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- unc_fixw_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("unc_fixw_an")

  return(res)
}

###############################################################
###############################################################
###############################################################
