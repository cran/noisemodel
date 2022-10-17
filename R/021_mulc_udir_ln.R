###############################################################
###############################################################
###############################################################

#' @export
mulc_udir_ln <- function(x, ...) UseMethod("mulc_udir_ln")

#' Multiple-class unidirectional label noise
#'
#' Introduction of \emph{Multiple-class unidirectional label noise} into a classification dataset.
#'
#' \emph{Multiple-class unidirectional label noise} introduction model randomly selects (\code{level}·100)\% of the samples
#' of each class \emph{c} with \code{goal}[c] != \code{NA}. Then, the labels \emph{c} of these samples are replaced by the class indicated in 
#' \code{goal}[c]. The order of indices in \code{goal} is determined by
#' \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param goal an integer vector with the indices of noisy classes for each class.
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
#' outdef <- mulc_udir_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1, 
#'                         goal = c(NA, 1, 2), order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- mulc_udir_ln(formula = Species ~ ., data = iris2D, level = 0.1, 
#'                         goal = c(NA, 1, 2), order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{minp_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name mulc_udir_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname mulc_udir_ln
mulc_udir_ln.default <- function(x, y, level, goal, order = levels(y), sortid = TRUE, ...){

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
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  num_noise <- 0
  idx_noise <- c()
  classes <- order
  
  for(c in 1:length(classes)){
    if(!is.na(goal[c])){
      values <- which(y == classes[c])
      nnoise <- round(length(values)*level)
      inoise <- sample(x = values, size = nnoise, replace = FALSE)
      
      num_noise <- num_noise + nnoise
      idx_noise <- c(idx_noise, inoise)
    }
  }
  if(sortid)
    idx_noise <- sort(idx_noise)
  

  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    for(i in 1:num_noise){
      cl <- as.integer(y[idx_noise[i]])
      y[idx_noise[i]] <- order[goal[cl]]
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("mulc_udir_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Multiple-class unidirectional label noise",
              param = list(level = level, goal = goal, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname mulc_udir_ln
#' @importFrom "stats" "model.frame"
mulc_udir_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- mulc_udir_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("mulc_udir_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
