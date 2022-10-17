###############################################################
###############################################################
###############################################################

#' @export
opes_idnn_ln <- function(x, ...) UseMethod("opes_idnn_ln")

#' Open-set ID/nearest-neighbor label noise
#'
#' Introduction of \emph{Open-set ID/nearest-neighbor label noise} into a classification dataset.
#'
#' \emph{Open-set ID/nearest-neighbor label noise} corrupts (\code{level}·100)\% of the samples with classes in \code{openset}. 
#' Then, the labels of these samples are replaced by 
#' the label of the nearest sample of a different in-distribution class. The order of the class 
#' labels for the indices in \code{openset} is determined by \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double with the noise level in [0,1] to be introduced.
#' @param openset an integer vector with the indices of classes in the open set (default: \code{c(1)}).
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
#' P. H. Seo, G. Kim, and B. Han. \strong{Combinatorial inference against label noise}. 
#' In \emph{Advances in Neural Information Processing Systems}, volume 32, pages 1171-1181, 2019.
#' url:\url{https://proceedings.neurips.cc/paper/2019/hash/0cb929eae7a499e50248a3a78f7acfc7-Abstract.html}.
#' 
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- opes_idnn_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                       level = 0.4, order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- opes_idnn_ln(formula = Species ~ ., data = iris2D, 
#'                       level = 0.4, order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{opes_idu_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name opes_idnn_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname opes_idnn_ln
opes_idnn_ln.default <- function(x, y, level, openset = c(1), order = levels(y), sortid = TRUE, ...){

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
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  idopen <- which(y %in% order[openset])
  idclose <- setdiff(1:nrow(x), idopen)
  
  num_noise <- round(length(idopen)*level)
  idx_noise <- sample(x = idopen, size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- order
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  if(num_noise > 0){
    rmopen <- setdiff(idopen, idx_noise)
    x <- x[-rmopen,]
    y <- y[-rmopen]
    idx_noise <- which(y %in% order[openset])
    idopen <- which(y %in% order[openset])
    idclose <- setdiff(1:nrow(x), idopen)
    
    newclasses <- rep(NA, num_noise)
    for(i in 1:num_noise){
      kmin <- 1
      nn <- get.knnx(data = x[idclose,], query = x[idx_noise[i],], k = kmin, algorithm = "brute")$nn.index
      nn_cla <- y[idclose][nn]
      majcla <- unname(which.max(table(nn_cla)))
      
      newclasses[i] <- majcla
    }
    
    newclasses <- order[newclasses]
    y[idx_noise] <- newclasses
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("opes_idnn_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Open-set ID/nearest-neighbor label noise",
              param = list(openset = openset, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname opes_idnn_ln
#' @importFrom "stats" "model.frame"
opes_idnn_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- opes_idnn_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("opes_idnn_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
