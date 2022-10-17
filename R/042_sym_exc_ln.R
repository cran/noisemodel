###############################################################
###############################################################
###############################################################

#' @export
sym_exc_ln <- function(x, ...) UseMethod("sym_exc_ln")

#' Symmetric exchange label noise
#'
#' Introduction of \emph{Symmetric exchange label noise} into a classification dataset.
#'
#' \emph{Symmetric exchange label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. These samples are divided into two groups: \emph{A} and \emph{B}. 
#' Then, each sample of group \emph{A} is labeled with the label of a sample of group \emph{B} and vice versa.
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
#' J. Schneider, J. P. Handali, and J. vom Brocke. \strong{Increasing trust in 
#' (big) data analytics}. In \emph{Proc. 2018 Advanced Information Systems 
#' Engineering Workshops}, volume 316 of LNBIP, pages 70-84, 2018.
#' \doi{10.1007/978-3-319-92898-2_6}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_exc_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_exc_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_cuni_ln}}, \code{\link{sym_cuni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_exc_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_exc_ln
sym_exc_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  xori <- x
  yori <- y
  
  num_noise <- round(nrow(x)*level)
  num_noise <- 2*round(num_noise/2)
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    grA <- sample(x = idx_noise, size = num_noise/2, replace = FALSE)
    grB <- setdiff(idx_noise, grA)
    
    lbA <- y[grA]
    lbB <- y[grB]
    lbC <- lbB
    
    lbB <- lbA
    lbA <- lbC
    
    y[grA] <- lbA
    y[grB] <- lbB
  }
  
  raux <- findnoise(xori, yori, x, y, "sym_exc_ln")

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_exc_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = raux$numnoise,
              idnoise = raux$idnoise,
              numclean = raux$numclean,
              idclean = raux$idclean,
              distr = distr,
              model = "Symmetric exchange label noise",
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
#' @rdname sym_exc_ln
#' @importFrom "stats" "model.frame"
sym_exc_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_exc_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_exc_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
