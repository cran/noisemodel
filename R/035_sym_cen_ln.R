###############################################################
###############################################################
###############################################################

#' @export
sym_cen_ln <- function(x, ...) UseMethod("sym_cen_ln")

#' Symmetric center-based label noise
#'
#' Introduction of \emph{Symmetric center-based label noise} into a classification dataset.
#'
#' \emph{Symmetric center-based label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. The probability for chosing the noisy label 
#' is determined based on the distance between class centers.
#' Thus, the mislabeling probability between classes increases as the distance between their 
#' centers decreases. This model is consistent with the intuition that samples in similar 
#' classes are more likely to be mislabeled. Besides, the model also allows mislabeling 
#' data in dissimilar classes with a relatively small probability, which corresponds to 
#' label noise caused by random errors.
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
#' X. Pu and C. Li. 
#' \strong{Probabilistic information-theoretic discriminant analysis for industrial 
#' label-noise fault diagnosis}. 
#' \emph{IEEE Transactions on Industrial Informatics}, 17(4):2664-2674, 2021.
#' \doi{10.1109/TII.2020.3001335}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_cen_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_cen_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{glev_uni_ln}}, \code{\link{sym_hienc_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_cen_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_cen_ln
sym_cen_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  num_noise <- round(nrow(x)*level)
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  # compute center of each class
  classes <- levels(y)
  center <- matrix(data = NA, nrow = length(classes), ncol = ncol(x))
  for(c1 in 1:length(classes)){
    values <- which(y == classes[c1])
    center[c1,] <- unname(apply(X = x[values,], MARGIN = 2, FUN = mean))
  }
  
  # compute distance between centers
  dist <- matrix(data = 0, nrow = length(classes), ncol = length(classes))
  for(c1 in 1:length(classes)){
    for(c2 in 1:length(classes)){
      if(c1 != c2){
        dist[c1,c2] <- sqrt(sum((center[c1,] - center[c2]) ^ 2))
      }
    }
  }
  
  # compute probabilities
  prob <- matrix(data = 0, nrow = length(classes), ncol = length(classes))
  for(c1 in 1:length(classes)){
    for(c2 in 1:length(classes)){
      if(c1 != c2){
        prob[c1,c2] <- (sqrt(1/dist[c1,c2]))/(sum(sqrt(1/dist[c1,-c1])))*level
      }
    }
  }
    
  # compute distribution
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  # select noisy values
  if(num_noise > 0){
    for(s in 1:num_noise){
      y[idx_noise[s]] <- sample(x = levels(y), size = 1, prob = prob[y[idx_noise[s]],])
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_cen_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Symmetric center-based label noise",
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
#' @rdname sym_cen_ln
#' @importFrom "stats" "model.frame"
sym_cen_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_cen_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_cen_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
