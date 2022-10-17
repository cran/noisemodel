###############################################################
###############################################################
###############################################################

#' @export
sym_nuni_ln <- function(x, ...) UseMethod("sym_nuni_ln")

#' Symmetric non-uniform label noise
#'
#' Introduction of \emph{Symmetric non-uniform label noise} into a classification dataset.
#'
#' \emph{Symmetric non-uniform label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. Then, the labels of these samples are randomly
#' replaced by other different ones according to the probabilities given in the transition matrix \code{tramat}. 
#' For details about the structure of the transition matrix, see Kang \emph{et al.} (2021).
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param tramat a double matrix with the values of the transition matrix.
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
#' J. Kang, R. Fernandez-Beltran, P. Duan, X. Kang, and A. J. Plaza.
#' \strong{Robust normalized softmax loss for deep metric learning-based characterization of 
#' remote sensing images with label noise}. 
#' \emph{IEEE Transactions on Geoscience and Remote Sensing}, 59(10):8798-8811, 2021.
#' \doi{10.1109/TGRS.2020.3042607}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' tramat <- matrix(data = c(0.9, 0.03, 0.07, 0.03, 0.9, 0.07, 0.03, 0.07, 0.9), 
#'                  nrow = 3, ncol = 3, byrow = TRUE)
#' outdef <- sym_nuni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                      level = 0.1, tramat = tramat)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_nuni_ln(formula = Species ~ ., data = iris2D, level = 0.1, tramat = tramat)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_adj_ln}}, \code{\link{sym_dran_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_nuni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_nuni_ln
sym_nuni_ln.default <- function(x, y, level, tramat, sortid = TRUE, ...){

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
  if(any(diag(tramat) != 1-level)){
    stop("elements in diag(tramat) should be equal to 1-level")
  }
  if(any(rowSums(tramat) != 1)){
    stop("rows in \"tramat\" should sum 1")
  }

  ######################################################
  # introduce noise #########
  num_noise <- round(nrow(x)*level)
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  tramat2 <- tramat
  diag(tramat2) <- 0
  
  # select noisy values
  if(num_noise > 0){
    for(s in 1:num_noise){
      y[idx_noise[s]] <- sample(x = levels(y), size = 1, prob = tramat2[y[idx_noise[s]],])
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_nuni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Symmetric non-uniform label noise",
              param = list(level = level, tramat = tramat, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_nuni_ln
#' @importFrom "stats" "model.frame"
sym_nuni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_nuni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_nuni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
