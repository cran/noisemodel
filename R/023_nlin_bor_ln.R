###############################################################
###############################################################
###############################################################

#' @export
nlin_bor_ln <- function(x, ...) UseMethod("nlin_bor_ln")

#' Non-linearwise borderline label noise
#'
#' Introduction of \emph{Non-linearwise borderline label noise} into a classification dataset.
#'
#' \emph{Non-linearwise borderline label noise} uses an SVM to induce the decision border 
#' in the dataset. Then, for each sample, its distance
#' to the decision border is computed. Finally, the
#' distances obtained are ordered in ascending order and the first (\code{level}·100)\% of them are used to determine the noisy samples. 
#' For each noisy sample, the majority class among its \code{k}-nearest neighbors of a different class 
#' is chosen as the new label.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param k an integer with the number of nearest neighbors to be used (default: 1).
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
#' L. P. F. Garcia, J. Lehmann, A. C. P. L. F. de Carvalho, and A. C. Lorena. 
#' \strong{New label noise injection methods for the evaluation of noise filters.} 
#' \emph{Knowledge-Based Systems}, 163:693–704, 2019.
#' \doi{10.1016/j.knosys.2018.09.031}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # usage of the default method
#' set.seed(9)
#' outdef <- nlin_bor_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#' 
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#' 
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- nlin_bor_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#' 
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References, considering a mislabeling process 
#' using the neighborhood of noisy samples.
#'
#' @seealso \code{\link{nei_bor_ln}}, \code{\link{ulap_bor_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name nlin_bor_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname nlin_bor_ln
#' @importFrom "stats" "predict"
nlin_bor_ln.default <- function(x, y, level, k = 1, sortid = TRUE, ...){

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
  if(k <= 0){
    stop("argument \"k\" must be higher than 0")
  }
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  dist <- bord_dist(x = x, y = y, krn = "radial")

  num_noise <- round(nrow(x)*level)
  ord <- sort(x = dist, decreasing = FALSE, index.return = TRUE)$ix
  if(num_noise > 0)
    idx_noise <- ord[1:num_noise]
  else
    idx_noise <- NULL

  # compute noise per class
  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  # introduce noise
  if(num_noise > 0){
    y <- bord_noise(x, y, num_noise, idx_noise, k)
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("nlin_bor_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Non-linearwise borderline label noise",
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
#' @rdname nlin_bor_ln
#' @importFrom "stats" "model.frame"
nlin_bor_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- nlin_bor_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("nlin_bor_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
