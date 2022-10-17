###############################################################
###############################################################
###############################################################

#' @export
mis_pre_ln <- function(x, ...) UseMethod("mis_pre_ln")

#' Misclassification prediction label noise
#'
#' Introduction of \emph{Misclassification prediction label noise} into a classification dataset.
#'
#' \emph{Misclassification prediction label noise} creates a Multi-Layer Perceptron (MLP) model from the dataset and relabels each
#' sample with the class predicted by the classifier.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
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
#' Q. Wang, B. Han, T. Liu, G. Niu, J. Yang, and C. Gong. 
#' \strong{Tackling instance-dependent label noise via a universal probabilistic model}. 
#' In \emph{Proc. 35th AAAI Conference on Artificial Intelligence}, pages 10183-10191, 2021.
#' url:\url{https://ojs.aaai.org/index.php/AAAI/article/view/17221}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- mis_pre_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)])
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- mis_pre_ln(formula = Species ~ ., data = iris2D)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{smam_bor_ln}}, \code{\link{nlin_bor_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name mis_pre_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname mis_pre_ln
#' @importFrom "caret" "train" "trainControl"
#' @importFrom "RSNNS" "mlp"
mis_pre_ln.default <- function(x, y, sortid = TRUE, ...){

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

  ######################################################
  # introduce noise #########
  if(any(sapply(x,is.factor))){
    x2 <- expandFactors(x)
  }
  else{
    x2 <- x
  }
  model <- train(x2, y, method = "mlp", trControl = trainControl(method = "none"))
  pred <- predict(model, newdata = x2)
  
  idx_noise <- which(y != pred)
  if(sortid)
    idx_noise <- sort(idx_noise)
  num_noise <- length(idx_noise)
  
  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    y[idx_noise] <- pred[idx_noise]
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("mis_pre_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Misclassification prediction label noise",
              param = list(sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname mis_pre_ln
#' @importFrom "stats" "model.frame"
mis_pre_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- mis_pre_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("mis_pre_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
