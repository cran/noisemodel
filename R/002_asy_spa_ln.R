###############################################################
###############################################################
###############################################################

#' @export
asy_spa_ln <- function(x, ...) UseMethod("asy_spa_ln")

#' Asymmetric sparse label noise
#'
#' Introduction of \emph{Asymmetric sparse label noise} into a classification dataset.
#'
#' \emph{Asymmetric sparse label noise} randomly selects (\code{levelO}·100)\% of the samples
#' in each odd class and (\code{levelE}·100)\% of the samples
#' in each even class -the order of the class labels is determined by
#' \code{order}. Then, each odd class is flipped to the next class, whereas each even class
#' is flipped to the previous class. If the dataset has an odd number of classes, the last class is not corrupted.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param levelO a double with the noise level in [0,1] to be introduced into each odd class.
#' @param levelE a double with the noise level in [0,1] to be introduced into each even class.
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
#' J. Wei and Y. Liu. 
#' \strong{When optimizing f-divergence is robust with label noise}. 
#' In \emph{Proc. 9th International Conference on Learning Representations}, pages 1-11, 2021.
#' url:\url{https://openreview.net/forum?id=WesiCoRVQ15}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- asy_spa_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                        levelO = 0.1, levelE = 0.3, order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- asy_spa_ln(formula = Species ~ ., data = iris2D, 
#'                         levelO = 0.1, levelE = 0.3, order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{mind_bdir_ln}}, \code{\link{fra_bdir_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name asy_spa_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname asy_spa_ln
asy_spa_ln.default <- function(x, y, levelO, levelE, order = levels(y), sortid = TRUE, ...){

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

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  num_noise <- 0
  idx_noise <- c()
  classes <- order
  
  level <- rep(0, length(classes))
  for(c in 1:length(classes)){
    if(c %% 2 == 0)
      level[c] <- levelE
    else if(c != length(classes))
      level[c] <- levelO
    
      values <- which(y == classes[c])
      nnoise <- round(length(values)*level[c])
      inoise <- sample(x = values, size = nnoise, replace = FALSE)
      
      num_noise <- num_noise + nnoise
      idx_noise <- c(idx_noise, inoise)
  }
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- order
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  if(num_noise > 0){
    aux <- as.integer(y[idx_noise])
    idd <- aux %% 2 == 1
    aux[idd] <- aux[idd] + 1
    newclasses <- aux
    
    aux <- as.integer(y[idx_noise])
    idd <- aux %% 2 == 0
    aux[idd] <- aux[idd] - 1
    newclasses[idd] <- aux[idd]

    newclasses <- order[newclasses]
    y[idx_noise] <- newclasses
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("asy_spa_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Asymmetric sparse label noise",
              param = list(levelO = levelO, levelE = levelE, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname asy_spa_ln
#' @importFrom "stats" "model.frame"
asy_spa_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- asy_spa_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("asy_spa_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
