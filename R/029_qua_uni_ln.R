###############################################################
###############################################################
###############################################################

#' @export
qua_uni_ln <- function(x, ...) UseMethod("qua_uni_ln")

#' Quadrant-based uniform label noise
#'
#' Introduction of \emph{Quadrant-based uniform label noise} into a classification dataset.
#'
#' For each sample, the probability of flipping its label is based on which quadrant 
#' (with respect to the attributes \code{att1} and \code{att2}) the sample falls in. 
#' The probability of mislabeling for each quadrant is expressed with the argument \code{level}, 
#' whose length is equal to 4. 
#' Let \emph{m1} and \emph{m2} be the mean values of the domain of \code{att1} and \code{att2}, respectively. 
#' Each quadrant is defined as follows: values <= \emph{m1} 
#' and <= \emph{m2} (first quadrant); values <= \emph{m1} and > \emph{m2} (second quadrant); 
#' values > \emph{m1} and <= \emph{m2} (third quadrant); and values > \emph{m1} 
#' and > \emph{m2} (fourth quadrant). Finally, the labels of these samples are randomly 
#' replaced by other different ones within the set of class labels.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double vector with the noise levels in [0,1] in each quadrant.
#' @param att1 an integer with the index of the first attribute forming the quadrants (default: 1).
#' @param att2 an integer with the index of the second attribute forming the quadrants (default: 2).
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
#' A. Ghosh, N. Manwani, and P. S. Sastry. 
#' \strong{Making risk minimization tolerant to label noise}. 
#' \emph{Neurocomputing}, 160:93-107, 2015.
#' \doi{10.1016/j.neucom.2014.09.081}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- qua_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                        level = c(0.05, 0.15, 0.20, 0.4))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- qua_uni_ln(formula = Species ~ ., data = iris2D, 
#'                         level = c(0.05, 0.15, 0.20, 0.4))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{exps_cuni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name qua_uni_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname qua_uni_ln
qua_uni_ln.default <- function(x, y, level, att1 = 1, att2 = 2, sortid = TRUE, ...){

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
  if(!is.numeric(x[,att1]) || !is.numeric(x[,att2])){
    stop("attributes \"att1\" and \"att2\" must be numeric")
  }

  ######################################################
  # compute samples in each quadrant #########
  m1 <- (max(x[,att1])+min(x[,att1]))/2
  m2 <- (max(x[,att2])+min(x[,att2]))/2
  
  a1l <- x[,att1] <= m1
  a1h <- x[,att1] > m1
  a2l <- x[,att2] <= m2
  a2h <- x[,att2] > m2
  
  q1 <- which(a1l & a2l)
  q2 <- which(a1l & a2h)
  q3 <- which(a1h & a2l)
  q4 <- which(a2h & a2h)
  
  #q1
  q1noise <- sample(x = q1, size = round(length(q1)*level[1]), replace = FALSE)
  q2noise <- sample(x = q2, size = round(length(q2)*level[2]), replace = FALSE)
  q3noise <- sample(x = q3, size = round(length(q3)*level[3]), replace = FALSE)
  q4noise <- sample(x = q4, size = round(length(q4)*level[4]), replace = FALSE)
  
  idx_noise <- c(q1noise, q2noise, q3noise, q4noise)
  num_noise <- length(idx_noise)
  if(sortid)
    idx_noise <- sort(idx_noise)

  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  if(num_noise > 0){
    newvalues <- sample_replace(x = 1:nlevels(y), size = num_noise, original = FALSE, ref = as.integer(y[idx_noise]))
    newvalues <- levels(y)[newvalues]
    y[idx_noise] <- newvalues
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("qua_uni_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Quadrant-based uniform label noise",
              param = list(level = level, att1 = att1, att2 = att2, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname qua_uni_ln
#' @importFrom "stats" "model.frame"
qua_uni_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- qua_uni_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("qua_uni_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
