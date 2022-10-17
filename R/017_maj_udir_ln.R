###############################################################
###############################################################
###############################################################

#' @export
maj_udir_ln <- function(x, ...) UseMethod("maj_udir_ln")

#' Majority-class unidirectional label noise
#'
#' Introduction of \emph{Majority-class unidirectional label noise} into a classification dataset.
#'
#' Let \emph{A} be the majority class and \emph{B} be the second majority class in the dataset.
#' The \emph{Majority-class unidirectional label noise} introduction model randomly selects (\code{level}·100)\% of the samples
#' of \emph{A} and labels them as \emph{B}.
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
#' J. Li, Q. Zhu, Q. Wu, Z. Zhang, Y. Gong, Z. He, and F. Zhu. 
#' \strong{SMOTE- NaN-DE: Addressing the noisy and borderline examples problem in imbalanced 
#' classification by natural neighbors and differential evolution}. 
#' \emph{Knowledge-Based Systems}, 223:107056, 2021.
#' \doi{10.1016/j.knosys.2021.107056}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- maj_udir_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- maj_udir_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References to multiclass data.
#'
#' @seealso \code{\link{asy_def_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name maj_udir_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname maj_udir_ln
maj_udir_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  freq <- table(y)
  idmax <- which.max(freq)
  maj <- names(idmax)

  freq_nofirst <- freq[-idmax]
  scnd <- names(which.max(freq_nofirst))

  id_maj <- which(y == maj)
  num_noise <- round(length(id_maj)*level)
  idx_noise <- sample(x = id_maj, size = num_noise, replace = FALSE)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(sortid)
    idx_noise <- sort(idx_noise)
  y[idx_noise] <- scnd

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("maj_udir_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Majority-class unidirectional label noise",
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
#' @rdname maj_udir_ln
#' @importFrom "stats" "model.frame"
maj_udir_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- maj_udir_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("maj_udir_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
