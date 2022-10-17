###############################################################
###############################################################
###############################################################

#' @export
smu_cuni_ln <- function(x, ...) UseMethod("smu_cuni_ln")

#' Smudge-based completely-uniform label noise
#'
#' Introduction of \emph{Smudge-based completely-uniform label noise} into a classification dataset.
#'
#' \emph{Smudge-based completely-uniform label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. Then, the labels of these samples are randomly
#' replaced by others within the set of class labels. An additional attribute 
#' \code{smudge} is included in the dataset with value equal to 1 in mislabeled samples and equal to 0 
#' in clean samples.
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
#' S. Thulasidasan, T. Bhattacharya, J. A. Bilmes, G. Chennupati, and J. Mohd-Yusof. 
#' \strong{Combating label noise in deep learning using abstention}. 
#' In \emph{Proc. 36th International Conference on Machine Learning}, volume 97 of PMLR, pages 6234-6243, 2019. 
#' url:\url{http://proceedings.mlr.press/v97/thulasidasan19a.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- smu_cuni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef, pca = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- smu_cuni_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{oned_uni_ln}}, \code{\link{attm_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name smu_cuni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname smu_cuni_ln
smu_cuni_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  # create smudge variable
  smudge <- rep(0, nrow(x))
  smudge[idx_noise] <- 1

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    newvalues <- sample_replace(x = 1:nlevels(y), size = num_noise, original = TRUE, ref = as.integer(y[idx_noise]))
    newvalues <- levels(y)[newvalues]
    y[idx_noise] <- newvalues
    raux <- findnoise(xori, yori, x, y, "smu_cuni_ln")
    x <- cbind(x, smudge)
  }
  else{
    raux <- list(numnoise = nnoiseclass, idnoise = list(idx_noise), numclean = distr-nnoiseclass, idclean = list(setdiff(1:nrow(x),idx_noise)))
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("smu_cuni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = raux$numnoise,
              idnoise = raux$idnoise,
              numclean = raux$numclean,
              idclean = raux$idclean,
              distr = distr,
              model = "Smudge-based completely-uniform label noise",
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
#' @rdname smu_cuni_ln
#' @importFrom "stats" "model.frame"
smu_cuni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- smu_cuni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("smu_cuni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
