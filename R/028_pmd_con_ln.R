###############################################################
###############################################################
###############################################################

#' @export
pmd_con_ln <- function(x, ...) UseMethod("pmd_con_ln")

#' PMD-based confidence label noise
#'
#' Introduction of \emph{PMD-based confidence label noise} into a classification dataset.
#'
#' \emph{PMD-based confidence label noise} approximates the probability of noise using 
#' the confidence prediction of a neural network. These predictions are used to estimate the 
#' mislabeling probability and the most possible noisy class label for each sample. Finally,
#' (\code{level}·100)\% of the samples in the dataset are randomly selected to be mislabeled
#' according to their values of probability computed.
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
#' Y. Zhang, S. Zheng, P. Wu, M. Goswami, and C. Chen. 
#' \strong{Learning with feature-dependent label noise: A progressive approach}. 
#' In \emph{Proc. 9th International Conference on Learning Representations}, pages 1-13, 2021.
#' url:\url{https://openreview.net/forum?id=ZPa2SyGcbwh}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- pmd_con_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- pmd_con_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{clu_vot_ln}}, \code{\link{sco_con_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name pmd_con_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname pmd_con_ln
#' @importFrom "lsr" "expandFactors"
#' @importFrom "nnet" "class.ind"
#' @importFrom "nnet" "nnet"
pmd_con_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  # train nnet #########
  nit <- 100
  x2 <- expandFactors(x)
  ideal <- class.ind(y)
  
  model <- nnet(x2, ideal, size = 10, softmax = TRUE, maxit = nit, trace = FALSE)
  resnn <- model$fitted.values
  resnnf <- resnn
  
  # compute confidences #########
  ux <- unname(apply(X = resnn, MARGIN = 1, FUN = max))
  clux <- unname(apply(X = resnn, MARGIN = 1, FUN = which.max))
  
  for(i in 1:nrow(resnn))
    resnn[i,clux[i]] <- -Inf
  
  sx <- unname(apply(X = resnn, MARGIN = 1, FUN = max))
  clsx <- unname(apply(X = resnn, MARGIN = 1, FUN = which.max))
  
  #compute probability
  prob <- rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    prob[i] <- (-0.5*(ux[i]-sx[i])^2)+0.5
  }
    
  num_noise <- round(nrow(x)*level)
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE, prob = prob)
  if(sortid)
    idx_noise <- sort(idx_noise)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    for(i in 1:num_noise){
      cl <- as.integer(y[idx_noise[i]])
      #cla4choose <- names(resnnf[idx_noise[i],-cl])
      cla4choose <- levels(y)[-cl]
      
      if(length(cla4choose) > 1)
        newvalue <- sample(x = cla4choose, size = 1, replace = FALSE, prob = resnnf[idx_noise[i],-cl])
      else
        newvalue <- cla4choose
      
      y[idx_noise[i]] <- newvalue
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("pmd_con_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "PMD-based confidence label noise",
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
#' @rdname pmd_con_ln
#' @importFrom "stats" "model.frame"
pmd_con_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- pmd_con_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("pmd_con_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
