###############################################################
###############################################################
###############################################################

#' @export
sigb_uni_ln <- function(x, ...) UseMethod("sigb_uni_ln")

#' Sigmoid-bounded uniform label noise
#'
#' Introduction of \emph{Sigmoid-bounded uniform label noise} into a classification dataset.
#'
#' \emph{Sigmoid-bounded uniform label noise} generates bounded instance-dependent and 
#' label-dependent label noise at random using a weight for each sample in 
#' the dataset to compute its noise probability through a sigmoid function. 
#' Note that this noise model considers the maximum noise level per class given by 
#' \code{level}, so the current noise level in each class may be lower than that specified. 
#' The order of the class labels is determined by \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double vector with the noise levels in [0,1] to be introduced into each class.
#' @param sortid a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).
#' @param order a character vector indicating the order of the classes (default: \code{levels(y)}).
#' @param formula a formula with the output class and, at least, one input attribute.
#' @param data a data frame in which to interpret the variables in the formula.
#' @param ... other options to pass to the function.
#'
#' @return An object of class \code{ndmodel} with elements:
#' \item{xnoise}{a data frame with the noisy input attributes.}
#' \item{ynoise}{a factor vector with the noisy output class.}
#' \item{numnoise}{an integer vector with the amount of noisy samples per class.}
#' \item{idnoise}{an integer vector list with the indices of noisy samples.}
#' \item{numclean}{an integer vector with the amount of clean samples per class.}
#' \item{idclean}{an integer vector list with the indices of clean samples.}
#' \item{distr}{an integer vector with the samples per class in the original data.}
#' \item{model}{the full name of the noise introduction model used.}
#' \item{param}{a list of the argument values.}
#' \item{call}{the function call.}
#'
#' @references
#' J. Cheng, T. Liu, K. Ramamohanarao, and D. Tao. 
#' \strong{Learning with bounded instance and label-dependent label noise}. 
#' In \emph{Proc. 37th International Conference on Machine Learning}, 
#' volume 119 of PMLR, pages 1789-1799, 2020.
#' url:\url{http://proceedings.mlr.press/v119/cheng20c.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sigb_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                       level = c(0.1, 0.2, 0.3))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sigb_uni_ln(formula = Species ~ ., data = iris2D, 
#'                       level = c(0.1, 0.2, 0.3))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References to multiclass data.
#'
#' @seealso \code{\link{larm_uni_ln}}, \code{\link{hubp_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sigb_uni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sigb_uni_ln
sigb_uni_ln.default <- function(x, y, level, order = levels(y), sortid = TRUE, ...){

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
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }
  if(any(level < 0) || any(level > 1)){
    stop("argument \"level\" must be in [0,1]")
  }
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }

  ######################################################
  y <- factor(y, levels = order)
  
  # compute weights #########
  xori <- x
  x <- scale(x)
  
  classes <- levels(y)
  w <- matrix(data = NA, nrow = length(classes), ncol = ncol(x)+1)
  for(c1 in 1:length(classes)){
    w[c1,] <- rnorm(n = ncol(x)+1, mean = 0, sd = 1)
  }
  
  # create data
  xp <- cbind(rep(1,nrow(x)), x)
  
  # compute noise probability per sample
  prob <- rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    ci <- as.integer(y[i])
    m1 <- matrix(data = w[ci,], nrow = 1, ncol = ncol(x)+1)
    m2 <- t(t(as.numeric(xp[i,])))
    mm <- as.numeric(m1 %*% m2)
    
    prob[i] <- level[ci]*(1/(1+exp(-mm)))
  }
  
  rnd <- runif(n = nrow(x), min = 0, max = 1)
  noise <- rnd < prob
  
  # select samples
  idx_noise <- which(noise)
  if(sortid)
    idx_noise <- sort(idx_noise)
  num_noise <- length(idx_noise)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    newvalues <- sample_replace(x = 1:nlevels(y), size = num_noise, original = FALSE, ref = as.integer(y[idx_noise]))
    newvalues <- levels(y)[newvalues]
    y[idx_noise] <- newvalues
  }
  
  x <- xori

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sigb_uni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Sigmoid-bounded uniform label noise",
              param = list(level = level, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sigb_uni_ln
#' @importFrom "stats" "model.frame"
sigb_uni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sigb_uni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sigb_uni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
