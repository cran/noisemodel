###############################################################
###############################################################
###############################################################

#' @export
smam_bor_ln <- function(x, ...) UseMethod("smam_bor_ln")

#' Small-margin borderline label noise
#'
#' Introduction of \emph{Small-margin borderline label noise} into a classification dataset.
#'
#' \emph{Small-margin borderline label noise} uses an SVM to induce the decision border 
#' in the dataset. For each sample, its distance
#' to the decision border is computed. Then, the samples are ordered according to their distance and 
#' (\code{level}·100)\% of the closest correctly classified samples to the decision boundary 
#' are selected to be mislabeled. For each noisy sample, the 
#' majority class among its \code{k}-nearest neighbors of a different class 
#' is chosen as the new label.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param k an integer with the number of nearest neighbors to be used (default: 1).
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
#' E. Amid, M. K. Warmuth, and S. Srinivasan. 
#' \strong{Two-temperature logistic regression based on the Tsallis divergence}.
#' In \emph{Proc. 22nd International Conference on Artificial Intelligence and Statistics}, 
#' volume 89 of PMLR, pages 2388-2396, 2019. 
#' url:\url{http://proceedings.mlr.press/v89/amid19a.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- smam_bor_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- smam_bor_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References to multiclass data, considering SVM with linear 
#' kernel as classifier and a mislabeling process using the neighborhood of noisy samples.
#'
#' @seealso \code{\link{nlin_bor_ln}}, \code{\link{nei_bor_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name smam_bor_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname smam_bor_ln
#' @importFrom "nnet" "multinom"
#' @importFrom "stats" "as.formula" "dexp"
smam_bor_ln.default <- function(x, y, level, k = 1, sortid = TRUE, ...){

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
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  # min distance
  mindist <- bord_dist(x = x, y = y)
  
  model <- svm(x = x, y = y, type = "C-classification", kernel = "linear")
  pred <- predict(model, x)
  mindist[pred != y] <- Inf
  
  idsrt <- sort(mindist, decreasing = FALSE, index.return = TRUE)$ix
  
  # get noisy samples
  num_noise <- round(nrow(x)*level)
  
  if(level > 0){
    idx_noise <- idsrt[1:num_noise]
  }
  else{
    idx_noise <- numeric(0)
  }
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  # introduce noise
  if(num_noise > 0){
    y <- bord_noise(x, y, num_noise, idx_noise, k)
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("smam_bor_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Small-margin borderline label noise",
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
#' @rdname smam_bor_ln
#' @importFrom "stats" "model.frame"
smam_bor_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- smam_bor_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("smam_bor_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
