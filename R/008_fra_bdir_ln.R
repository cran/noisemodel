###############################################################
###############################################################
###############################################################

#' @export
fra_bdir_ln <- function(x, ...) UseMethod("fra_bdir_ln")

#' Fraud bidirectional label noise
#'
#' Introduction of \emph{Fraud bidirectional label noise} into a classification dataset.
#'
#' \emph{Fraud bidirectional label noise} randomly selects (\code{level}·100)\% of the samples
#' from the minority class in the dataset and \code{level}·10 samples from the majority class.
#' Then, minority class samples are mislabeled as belonging to the majority class and majority class 
#' samples are mislabeled as belonging to the minority class. In case of ties determining minority and majority classes, 
#' a random class is chosen among them.
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
#' Z. Salekshahrezaee, J. L. Leevy, and T. M. Khoshgoftaar. 
#' \strong{A reconstruction error-based framework for label noise detection}. 
#' \emph{Journal of Big Data}, 8(1):1-16, 2021.
#' \doi{10.1186/s40537-021-00447-5}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- fra_bdir_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- fra_bdir_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{irs_bdir_ln}}, \code{\link{pai_bdir_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name fra_bdir_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname fra_bdir_ln
fra_bdir_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  classes <- levels(y)
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  distr2 <- distr
  mc <- which.min(distr2)
  distr2[mc] <- -Inf
  Mc <- which.max(distr2)
  
  # introduce noise #########
  num_pos <- unname(round(distr[mc]*level))
  num_neg <- round(level*10)
  
  # min cla
  values <- which(y == classes[mc])
  inoisem <- sample(x = values, size = num_pos, replace = FALSE)
  values <- which(y == classes[Mc])
  inoiseM <- sample(x = values, size = num_neg, replace = FALSE)
  
  idx_noise <- c(inoisem, inoiseM)
  if(sortid)
    idx_noise <- sort(idx_noise)
  num_noise <- num_pos + num_neg
  
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  
  if(num_noise > 0){
    y[inoisem] <- levels(y)[Mc]
    y[inoiseM] <- levels(y)[mc]
  }
  
  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("fra_bdir_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Fraud bidirectional label noise",
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
#' @rdname fra_bdir_ln
#' @importFrom "stats" "model.frame"
fra_bdir_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- fra_bdir_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("fra_bdir_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
