###############################################################
###############################################################
###############################################################

#' @export
oned_uni_ln <- function(x, ...) UseMethod("oned_uni_ln")

#' One-dimensional uniform label noise
#'
#' Introduction of \emph{One-dimensional uniform label noise} into a classification dataset.
#'
#' \emph{One-dimensional uniform label noise} is based on the introduction of noise 
#' according to the values of the attribute \code{att}. Samples of class \emph{i} with  
#' the attribute \code{att} falling between \code{lower}[i] and \code{upper}[i] 
#' have a probability \code{level} of being mislabeled. The labels of these samples are randomly
#' replaced by other different ones within the set of class labels. The order of the class labels is 
#' determined by \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param att an integer with the index of the attribute determining noisy samples.
#' @param lower a vector with the lower bound to determine the noisy region of each class.
#' @param upper a vector with the upper bound to determine the noisy region of each class.
#' @param order a character vector indicating the order of the classes (default: \code{levels(y)}).
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
#' N. Gornitz, A. Porbadnigk, A. Binder, C. Sannelli, M. L. Braun, K. Muller, and M. Kloft. 
#' \strong{Learning and evaluation in presence of non-i.i.d. label noise}. 
#' In \emph{Proc. 17th International Conference on Artificial Intelligence and Statistics}, 
#' volume 33 of PMLR, pages 293–302, 2014. url:\url{https://proceedings.mlr.press/v33/gornitz14.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # usage of the default method
#' set.seed(9)
#' outdef <- oned_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                     level = 0.5, att = 1, lower = c(1.5,2,6), upper = c(2,4,7))
#' 
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#' 
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- oned_uni_ln(formula = Species ~ ., data = iris2D, 
#'                     level = 0.5, att = 1, lower = c(1.5,2,6), upper = c(2,4,7))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References to multiclass data, considering a 
#' noise level to control the number of errors in the data
#'
#' @seealso \code{\link{attm_uni_ln}}, \code{\link{qua_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name oned_uni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname oned_uni_ln
oned_uni_ln.default <- function(x, y, level, att, lower, upper, order = levels(y), sortid = TRUE, ...){

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
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }
  if(!is.numeric(x[,att])){
    stop("attribute \"att\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  idx_noise <- c()
  for(c in 1:nlevels(y)){
    samplescla <- which(as.integer(y) == c)
    noisecla <- samplescla[x[samplescla,att] >= lower[c] & x[samplescla,att]<=upper[c]]
    idnx <- sample(x = noisecla, size = round(level*length(noisecla)), replace = FALSE)
    idx_noise <- c(idx_noise, idnx)
  }
  
  num_noise <- length(idx_noise)
  if(sortid)
    idx_noise <- sort(idx_noise)

  classes <- order
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    newvalues <- sample_replace(x = 1:nlevels(y), size = num_noise, original = FALSE, ref = as.integer(y[idx_noise]))
    newvalues <- order[newvalues]
    y[idx_noise] <- newvalues
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("oned_uni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "One-dimensional uniform label noise",
              param = list(att = att, lower = lower, upper = upper, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname oned_uni_ln
#' @importFrom "stats" "model.frame"
oned_uni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- oned_uni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("oned_uni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
