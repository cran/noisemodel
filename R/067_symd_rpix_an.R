###############################################################
###############################################################
###############################################################

#' @export
symd_rpix_an <- function(x, ...) UseMethod("symd_rpix_an")

#' Symmetric/dependent random-pixel attribute noise
#'
#' Introduction of \emph{Symmetric/dependent random-pixel attribute noise} into a classification dataset.
#'
#' \emph{Symmetric/dependent random-pixel attribute noise} corrupts (\code{level}·100)\% 
#' of the samples in the dataset. 
#' For each sample, its attribute values are shuffled using independent random permutations.
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
#' L. Huang, C. Zhang, and H. Zhang.
#' \strong{Self-adaptive training: Beyond empirical risk minimization}.
#' In \emph{Proceedings of the Advances in Neural Information Processing Systems}, 2020, Vol. 33, pp. 19365–19376.
#' \url{https://proceedings.neurips.cc/paper/2020/file/e0ab531ec312161511493b002f9be2ee-Paper.pdf}
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- symd_rpix_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- symd_rpix_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{unc_fixw_an}}, \code{\link{sym_end_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name symd_rpix_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname symd_rpix_an
symd_rpix_an.default <- function(x, y, level, sortid = TRUE, ...){

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

  ######################################################
  # introduce noise #########
  xori <- x
  yori <- y
  
  num_noise <- rep(round(level*nrow(x)), ncol(x))
  idx_noise <- list()
  idx_clean <- list()
  
  idnall <- sample(1:nrow(x), num_noise[1], replace = FALSE)
  for(a in 1:ncol(x)){
    idx_noise[[a]] <- idnall
    idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
  }
  
  if(level > 0){
    for(i in 1:length(idnall)){
      ii <- idnall[i]
      neworder <- sample(x = 1:ncol(x), size = ncol(x), replace = FALSE)
      x[ii,] <- unname(x[ii,neworder])
    } 
  }
  
  classes <- levels(y)
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  raux <- findnoise(xori, yori, x, y, "symd_rpix_an")

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("symd_rpix_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = raux$numnoise,
              idnoise = raux$idnoise,
              numclean = raux$numclean,
              idclean = raux$idclean,
              distr = distr,
              model = "Symmetric/dependent random-pixel attribute noise",
              param = list(sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname symd_rpix_an
#' @importFrom "stats" "model.frame"
symd_rpix_an.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- symd_rpix_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("symd_rpix_an")

  return(res)
}

###############################################################
###############################################################
###############################################################
