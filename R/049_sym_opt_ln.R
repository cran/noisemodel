###############################################################
###############################################################
###############################################################

#' @export
sym_opt_ln <- function(x, ...) UseMethod("sym_opt_ln")

#' Symmetric optimistic label noise
#'
#' Introduction of \emph{Symmetric optimistic label noise} into a classification dataset.
#'
#' \emph{Symmetric optimistic label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. 
#' In the optimistic case, the probability of a class \emph{i} of being mislabeled as class \emph{j} is 
#' higher for \emph{j} > \emph{i} in comparison to \emph{j} < \emph{i}.
#' Thus, when noise for a certain class occurs, it is assigned to a random higher class with probability \code{levelH} 
#' and to a random lower class with probability 1-\code{levelH}. The order of the classes is determined by 
#' \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param levelH a double in (0.5, 1] with the noise level for higher classes (default: 0.9).
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
#' R. C. Prati, J. Luengo, and F. Herrera. 
#' \strong{Emerging topics and challenges of learning from noisy data in nonstandard classification: 
#' a survey beyond binary class noise}. \emph{Knowledge and Information Systems}, 60(1):63–97, 2019.
#' \doi{10.1007/s10115-018-1244-4}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_opt_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                      level = 0.1, order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_opt_ln(formula = Species ~ ., data = iris2D, 
#'                      level = 0.1, order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_usim_ln}}, \code{\link{sym_natd_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_opt_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_opt_ln
sym_opt_ln.default <- function(x, y, level, levelH = 0.9, order = levels(y), sortid = TRUE, ...){

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
  if(level < 0 || level > 1){
    stop("argument \"level\" must be in [0,1]")
  }
  if(levelH <= 0.5 || levelH > 1){
    stop("argument \"levelH\" must be in (0.5,1]")
  }
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  num_noise <- round(nrow(x)*level)
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- order
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    for(s in 1:num_noise){
      cl <- as.integer(y[idx_noise[s]])
      
      # higher class
      if( (runif(1) < levelH && cl < length(classes)) || cl == 1 ){
        restc <- seq(from = cl+1, to = length(classes), by = 1)
        newcla <- safe_sample(x = restc, size = 1)
      }
      # lower class
      else{
        restc <- seq(from = 1, to = cl-1, by = 1)
        newcla <- safe_sample(x = restc, size = 1)
      }
      
      y[idx_noise[s]] <- order[newcla]
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_opt_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Symmetric optimistic label noise",
              param = list(level = level, levelH = levelH, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_opt_ln
#' @importFrom "stats" "model.frame"
sym_opt_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_opt_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_opt_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
