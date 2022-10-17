###############################################################
###############################################################
###############################################################

#' @export
ugau_bor_ln <- function(x, ...) UseMethod("ugau_bor_ln")

#' Uneven-Gaussian borderline label noise
#'
#' Introduction of \emph{Uneven-Gaussian borderline label noise} into a classification dataset.
#'
#' \emph{Uneven-Gaussian borderline label noise} uses an SVM to induce the decision border 
#' in the dataset. For each sample, its distance
#' to the decision border is computed. Then, a Gaussian distribution with parameters (\code{mean}, \code{sd}) is 
#' used to compute the value for the probability density function associated to each distance. 
#' For each class \emph{c}[i], it randomly selects (\code{level}[i]·100)\% of the samples
#' in the dataset based on their values of the probability density function -the order of the class labels is determined by
#' \code{order}. For each noisy sample, the 
#' majority class among its \code{k}-nearest neighbors of a different class 
#' is chosen as the new label.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double vector with the noise levels in [0,1] to be introduced into each class.
#' @param mean a double with the mean for the Gaussian distribution (default: 0).
#' @param sd a double with the standard deviation for the Gaussian distribution (default: 1).
#' @param k an integer with the number of nearest neighbors to be used (default: 1).
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
#' J. Du and Z. Cai. 
#' \strong{Modelling class noise with symmetric and asymmetric distributions}. 
#' In \emph{Proc. 29th AAAI Conference on Artificial Intelligence}, pages 2589-2595, 2015.
#' url:\url{https://dl.acm.org/doi/10.5555/2886521.2886681}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- ugau_bor_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                       level = c(0.1, 0.2, 0.3), order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- ugau_bor_ln(formula = Species ~ ., data = iris2D,
#'                       level = c(0.1, 0.2, 0.3), order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References to multiclass data, considering SVM with linear 
#' kernel as classifier, a mislabeling process using the neighborhood of noisy samples and a 
#' noise level to control the number of errors in the data.
#'
#' @seealso \code{\link{gaum_bor_ln}}, \code{\link{gau_bor_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name ugau_bor_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname ugau_bor_ln
#' @importFrom "stats" "as.formula" "dnorm"
ugau_bor_ln.default <- function(x, y, level, mean = 0, sd = 1, k = 1, order = levels(y), sortid = TRUE, ...){

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
  if(any(level < 0) || any(level > 1)){
    stop("argument \"level\" must be in [0,1]")
  }
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }
  if(sd < 0){
    stop("argument \"sd\" must be higher than 0")
  }
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  mindist <- bord_dist(x = x, y = y)

  # density function
  denfun <- rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    denfun[i] <- dnorm(x = mindist[i], mean = mean, sd = sd)
  }
  
  # select noisy samples
  num_noise <- 0
  idx_noise <- c()
  classes <- order
  for(c in 1:length(classes)){
    values <- which(y == classes[c])
    nnoise <- round(length(values)*level[c])
    inoise <- sample(x = values, size = nnoise, replace = FALSE, prob = denfun[values])
    
    num_noise <- num_noise + nnoise
    idx_noise <- c(idx_noise, inoise)
  }
  if(sortid)
    idx_noise <- sort(idx_noise)
  

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
  call[[1]] <- as.name("ugau_bor_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Uneven-Gaussian borderline label noise",
              param = list(level = level, mean = mean, sd = sd, k = k, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname ugau_bor_ln
#' @importFrom "stats" "model.frame"
ugau_bor_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- ugau_bor_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("ugau_bor_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
