###############################################################
###############################################################
###############################################################

#' @export
asy_def_ln <- function(x, ...) UseMethod("asy_def_ln")

#' Asymmetric default label noise
#'
#' Introduction of \emph{Asymmetric default label noise} into a classification dataset.
#'
#' \emph{Asymmetric default label noise} randomly selects (\code{level}[i]·100)\% of the samples
#' of each class \emph{C}[i] in the dataset -the order of the class labels is determined by
#' \code{order}. Then, the labels of these samples are
#' replaced by a fixed label (\emph{C}[\code{def}]) within the set of class labels.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double vector with the noise levels in [0,1] to be introduced into each class.
#' @param def an integer with the index of the default class (default: 1).
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
#' R. C. Prati, J. Luengo, and F. Herrera. 
#' \strong{Emerging topics and challenges of learning from noisy data in nonstandard classification: 
#' a survey beyond binary class noise}. \emph{Knowledge and Information Systems}, 60(1):63–97, 2019.
#' \doi{10.1007/s10115-018-1244-4}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- asy_def_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                      level = c(0.1, 0.2, 0.3), order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- asy_def_ln(formula = Species ~ ., data = iris2D, 
#'                      level = c(0.1, 0.2, 0.3), order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_nean_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name asy_def_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname asy_def_ln
asy_def_ln.default <- function(x, y, level, def = 1, order = levels(y), sortid = TRUE, ...){

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
  if(any(level < 0) || any(level > 1)){
    stop("argument \"level\" must be in [0,1]")
  }
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  num_noise <- 0
  idx_noise <- c()
  classes <- order
  
  for(c in 1:length(classes)){
    if(c != def){
      values <- which(y == classes[c])
      nnoise <- round(length(values)*level[c])
      inoise <- sample(x = values, size = nnoise, replace = FALSE)
      
      num_noise <- num_noise + nnoise
      idx_noise <- c(idx_noise, inoise)
    }
  }
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  if(num_noise > 0){
    y[idx_noise] <- order[def]
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("asy_def_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Asymmetric default label noise",
              param = list(level = level, def = def, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname asy_def_ln
#' @importFrom "stats" "model.frame"
asy_def_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- asy_def_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("asy_def_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
