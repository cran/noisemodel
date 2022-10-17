###############################################################
###############################################################
###############################################################

#' @export
exps_cuni_ln <- function(x, ...) UseMethod("exps_cuni_ln")

#' Exponential/smudge completely-uniform label noise
#'
#' Introduction of \emph{Exponential/smudge completely-uniform label noise} into a classification dataset.
#'
#' \emph{Exponential/smudge completely-uniform label noise} includes an additional attribute (\emph{smudge}) in the dataset with 
#' random values in [0,1]. This attribute is used to compute the mislabeling probability for each sample
#' based on an exponential function (in which \code{level} is used as lambda). It selects samples
#' in the dataset based on these probabilities. Finally, the labels of these samples are 
#' randomly replaced by others within the set of class labels (this model can choose the original 
#' label of a sample as noisy).
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the lambda value.
#' @param sortid a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).
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
#' B. Denham, R. Pears, and M. A. Naeem. 
#' \strong{Null-labelling: A generic approach for learning in the presence of class noise}. 
#' In \emph{Proc. 20th IEEE International Conference on Data Mining}, pages 990–995, 2020.
#' \doi{10.1109/ICDM50108.2020.00114}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- exps_cuni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.8)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef, pca = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- exps_cuni_ln(formula = Species ~ ., data = iris2D, level = 0.8)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{opes_idu_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name exps_cuni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname exps_cuni_ln
exps_cuni_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  yori <- y
  
  # create a normalized smudge variable
  smudge <- runif(n = nrow(x))
  s <- -(log(1-0.95)/level)
  prob <- level*exp(-level*s*(1-smudge))
  
  # generate noisy samples
  r <- runif(n = nrow(x))
  idx_noise <- which(r <= prob)
  num_noise <- length(idx_noise)
  if(sortid)
    idx_noise <- sort(idx_noise)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    newvalues <- sample_replace(x = 1:nlevels(y), size = num_noise, original = TRUE, ref = as.integer(y[idx_noise]))
    newvalues <- levels(y)[newvalues]
    y[idx_noise] <- newvalues
    x <- cbind(x, smudge)
  }
  
  idx_noise <- which(y != yori)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("exps_cuni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Exponential/smudge completely-uniform label noise",
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
#' @rdname exps_cuni_ln
#' @importFrom "stats" "model.frame"
exps_cuni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- exps_cuni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("exps_cuni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
