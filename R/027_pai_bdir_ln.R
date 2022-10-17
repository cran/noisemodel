###############################################################
###############################################################
###############################################################

#' @export
pai_bdir_ln <- function(x, ...) UseMethod("pai_bdir_ln")

#' Pairwise bidirectional label noise
#'
#' Introduction of \emph{Pairwise bidirectional label noise} into a classification dataset.
#'
#' For each vector (\emph{c1}, \emph{c2}) in \code{pairs}, 
#' \emph{Pairwise bidirectional label noise} randomly selects (\code{level}·100)\% of the samples
#' from class \emph{c1} in the dataset and (\code{level}·100)\% of the samples from class
#' \emph{c2}. Then, \emph{c1} samples are mislabeled as belonging to \emph{c2} and 
#' \emph{c2} samples are mislabeled as belonging to \emph{c1}. The order of the class labels is 
#' determined by \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param pairs a list of integer vectors with the indices of classes to corrupt.
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
#' S. Fefilatyev, M. Shreve, K. Kramer, L. O. Hall, D. B. Goldgof, R. Kasturi, K. Daly, A. Remsen, and H. Bunke. 
#' \strong{Label-noise reduction with support vector machines}. 
#' In \emph{Proc. 21st International Conference on Pattern Recognition}, pages 3504-3508, 2012.
#' url:\url{https://ieeexplore.ieee.org/document/6460920/}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # create new class with some samples
#' class <- as.character(iris2D$Species)
#' class[iris2D$Petal.Length > 6] <- "newclass"
#' iris2D$Species <- as.factor(class)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- pai_bdir_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                       level = 0.1, pairs = list(c(1,2), c(3,4)), 
#'                       order = c("virginica", "setosa", "newclass", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- pai_bdir_ln(formula = Species ~ ., data = iris2D, 
#'                       level = 0.1, pairs = list(c(1,2), c(3,4)), 
#'                       order = c("virginica", "setosa", "newclass", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name pai_bdir_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname pai_bdir_ln
pai_bdir_ln.default <- function(x, y, level, pairs, order = levels(y), sortid = TRUE, ...){

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
  if(length(unique(unlist(pairs))) != length(pairs)*2){
    stop("each class must be in a single vector pair in \"pairs\"")
  }
  if(any(unique(unlist(pairs)) < 1) || any(unique(unlist(pairs)) > nlevels(y))){
    stop("indices in \"pairs\" must be in [1,nlevels(y)]")
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
  
  yn <- as.character(y)
  for(c in 1:length(pairs)){
    c1 <- pairs[[c]][1]
    c2 <- pairs[[c]][2]
    
    values_c1 <- which(y == classes[c1])
    naux <- round(length(values_c1)*level)
    values_c1 <- sample(x = values_c1, size = naux, replace = FALSE)
    yn[values_c1] <- classes[c2]
    num_noise <- num_noise + length(values_c1)
    idx_noise <- c(idx_noise, values_c1)
    
    values_c2 <- which(y == classes[c2])
    naux <- round(length(values_c2)*level)
    values_c2 <- sample(x = values_c2, size = naux, replace = FALSE)
    yn[values_c2] <- classes[c1]
    num_noise <- num_noise + length(values_c2)
    idx_noise <- c(idx_noise, values_c2)
  }
  
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- order
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  if(num_noise > 0){
    y <- as.factor(yn)
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("pai_bdir_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Pairwise bidirectional label noise",
              param = list(level = level, pairs = pairs, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname pai_bdir_ln
#' @importFrom "stats" "model.frame"
pai_bdir_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- pai_bdir_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("pai_bdir_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
