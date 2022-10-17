###############################################################
###############################################################
###############################################################

#' @export
boud_gau_an <- function(x, ...) UseMethod("boud_gau_an")

#' Boundary/dependent Gaussian attribute noise
#'
#' Introduction of \emph{Boundary/dependent Gaussian attribute noise} into a classification dataset.
#'
#' \emph{Boundary/dependent Gaussian attribute noise} corrupts (\code{level}·100)\% samples among the 
#' ((\code{level}+0.1)·100)\% of samples closest to the decision boundary. Their attribute values are corrupted by adding a random number 
#' that follows a Gaussian distribution of \emph{mean} = 0 and \emph{standard deviation} = (max-min)·\code{k}, being
#' \emph{max} and \emph{min} the limits of the attribute domain. For nominal attributes, a random value is chosen.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param k a double in [0,1] with the scale used for the standard deviation (default: 0.2).
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
#' J. Bi and T. Zhang. 
#' \strong{Support vector classification with input data uncertainty}. 
#' In \emph{Advances in Neural Information Processing Systems}, volume 17, pages 161-168, 2004.
#' url:\url{https://proceedings.neurips.cc/paper/2004/hash/22b1f2e0983160db6f7bb9f62f4dbb39-Abstract.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- boud_gau_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- boud_gau_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{imp_int_an}}, \code{\link{asy_int_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name boud_gau_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname boud_gau_an
#' @importFrom "stats" "runif"
#' @importFrom "stats" "as.formula"
boud_gau_an.default <- function(x, y, level, k = 0.2, sortid = TRUE, ...){
  
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
  mindist <- bord_dist(x = x, y = y)
  
  # sort distances
  ids <- sort(x = mindist, decreasing = FALSE, index.return = TRUE)$ix
  
  # compute noise
  num_noise <- round(level*nrow(x))
  num_total <- round((level+0.1)*nrow(x))
  if(num_total > nrow(x))
    num_total <- nrow(x)
  
  # select samples
  sam <- sample(x = ids[1:num_total], size = num_noise, replace = FALSE)
  
  idx_noise <- list()
  idx_clean <- list()
  
  if(num_noise > 0){
    for(a in 1:ncol(x)){
      
      idx_noise[[a]] <- sam
      if(sortid){
        idx_noise[[a]] <- sort(idx_noise[[a]])
      }
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
      
      if(is.numeric(x[,a])){
        newvalues <- x[idx_noise[[a]],a] + rnorm(n = num_noise, mean = 0, sd = (max(x[,a])-min(x[,a]))*k)
        if(is.integer(x[,a])){
          newvalues <- round(newvalues)
        }
        newvalues[newvalues < min(x[,a])] <- min(x[,a])
        newvalues[newvalues > max(x[,a])] <- max(x[,a])
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
  call[[1]] <- as.name("boud_gau_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = rep(num_noise,ncol(x)),
              idnoise = idx_noise,
              numclean = rep(nrow(x),ncol(x))-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Boundary/dependent Gaussian attribute noise",
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
#' @rdname boud_gau_an
#' @importFrom "stats" "model.frame"
boud_gau_an.formula <- function(formula, data, ...){
  
  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }
  
  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL
  
  x <- mf[,-1]
  y <- mf[,1]
  
  res <- boud_gau_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("boud_gau_an")
  
  return(res)
}

###############################################################
###############################################################
###############################################################
