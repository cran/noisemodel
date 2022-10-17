###############################################################
###############################################################
###############################################################

#' @export
sym_dran_ln <- function(x, ...) UseMethod("sym_dran_ln")

#' Symmetric double-random label noise
#'
#' Introduction of \emph{Symmetric double-random label noise} into a classification dataset.
#'
#' \emph{Symmetric double-random label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. Then, each of the original class labels is 
#' flipped to one between two other random labels.
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
#' A. Ghosh and A. S. Lan. 
#' \strong{Do we really need gold samples for sample weighting under label noise?} 
#' In \emph{Proc. 2021 IEEE Winter Conference on Applications of Computer Vision}, pages 3921-3930, 2021.
#' \doi{10.1109/WACV48630.2021.00397}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_dran_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_dran_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_hie_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_dran_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_dran_ln
sym_dran_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  defmat <- matrix(data = NA, nrow = length(classes), ncol = 2)
  for(c in 1:length(classes)){
    defmat[c,] <- safe_sample(x = (1:length(classes))[-c], size = min(nlevels(y)-1,2), replace = FALSE)
  }
  
  idsel <- 1:nrow(x)
  num_noise <- round(length(idsel)*level)
  idx_noise <- sample(x = idsel, size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  if(num_noise > 0){
    for(i in 1:num_noise){
      
      #get class
      cl <- as.integer(y[idx_noise[i]])
      def <- defmat[cl,1]
      def2 <- defmat[cl,2]
      
      s <- sample(x = c(TRUE, FALSE), size = 1, replace = FALSE)
      if(s)
        y[idx_noise[i]] <- levels(y)[def]
      else
        y[idx_noise[i]] <- levels(y)[def2]
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_dran_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Symmetric double-random label noise",
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
#' @rdname sym_dran_ln
#' @importFrom "stats" "model.frame"
sym_dran_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_dran_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_dran_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
