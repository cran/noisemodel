###############################################################
###############################################################
###############################################################

#' @export
hubp_uni_ln <- function(x, ...) UseMethod("hubp_uni_ln")

#' Hubness-proportional uniform label noise
#'
#' Introduction of \emph{Hubness-proportional uniform label noise} into a classification dataset.
#'
#' \emph{Hubness-proportional uniform label noise} is based on the presence of hubs
#' in the dataset. It selects (\code{level}·100)\% of the samples in the dataset using a 
#' discrete probability distribution based on the concept of hubness, which is computed 
#' using the nearest neighbors of each sample. Then, the class labels
#' of these samples are randomly replaced by different ones from the \emph{c} classes.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param k an integer with the number of neighbors to compute the hubness of each sample (default: 3).
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
#' N. Tomasev and K. Buza. 
#' \strong{Hubness-aware kNN classification of high-dimensional data in presence of label noise}. 
#' \emph{Neurocomputing}, 160:157-172, 2015. 
#' \doi{10.1016/j.neucom.2014.10.084}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- hubp_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- hubp_uni_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{smu_cuni_ln}}, \code{\link{oned_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name hubp_uni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname hubp_uni_ln
#' @importFrom "FNN" "get.knnx"
hubp_uni_ln.default <- function(x, y, level, k = 3, sortid = TRUE, ...){

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
  if(k < 1){
    stop("argument \"k\" must be higher than 0")
  }
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  nn <- get.knnx(data = x, query = x, k = k + 1, algorithm = "brute")$nn.index[,2:(k+1)]

  freq <- table(nn)
  idx <- as.integer(names(freq))

  hubness <- rep(0, nrow(x))
  hubness[idx] <- as.integer(freq)

  prob <- rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    prob[i] <- (hubness[i]+1)/((k+1)*nrow(x))
  }

  num_noise <- round(nrow(x)*level)
  idx_noise <- sample(1:nrow(x), size=num_noise, replace=FALSE, prob=prob)

  if(sortid)
    idx_noise <- sort(idx_noise)

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

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("hubp_uni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Hubness-proportional uniform label noise",
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
#' @rdname hubp_uni_ln
#' @importFrom "stats" "model.frame"
hubp_uni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("data: must be a data.frame")
  }

  mf <- model.frame(formula,data) # column 1 is the class variable
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- hubp_uni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("hubp_uni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
