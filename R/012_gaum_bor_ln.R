###############################################################
###############################################################
###############################################################

#' @export
gaum_bor_ln <- function(x, ...) UseMethod("gaum_bor_ln")

#' Gaussian-mixture borderline label noise
#'
#' Introduction of \emph{Gaussian-mixture borderline label noise} into a classification dataset.
#'
#' \emph{Gaussian-mixture borderline label noise} uses an SVM to induce the decision border 
#' in the dataset. For each sample, its distance to the decision border is computed. 
#' Then, a Gaussian mixture distribution with parameters (\code{mean}, \code{sd}) and weights \code{w} 
#' is used to compute the value for the probability density function
#' associated to each distance. Finally,
#' (\code{level}·100)\% of the samples in the dataset are randomly selected to be mislabeled
#' according to their values of the probability density function. For each noisy sample, the 
#' majority class among its \code{k}-nearest neighbors of a different class 
#' is chosen as the new label.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param mean a double vector with the mean for each Gaussian distribution (default: \code{c}(0,2)).
#' @param sd a double vector with the standard deviation for each Gaussian distribution (default: \code{c}(\code{sqrt}(0.5),\code{sqrt}(0.5))).
#' @param w a double vector with the weight for each Gaussian distribution (default: \code{c}(0.5,0.5)).
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
#' J. Bootkrajang and J. Chaijaruwanich. 
#' \strong{Towards instance-dependent label noise-tolerant classification: a probabilistic approach}. 
#' \emph{Pattern Analysis and Applications}, 23(1):95-111, 2020.
#' \doi{10.1007/s10044-018-0750-z}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # usage of the default method
#' set.seed(9)
#' outdef <- gaum_bor_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#' 
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#' 
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- gaum_bor_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#' 
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#'
#' @note Noise model adapted from the papers in References, considering SVM with linear 
#' kernel as classifier, a mislabeling process using the neighborhood of noisy samples and a 
#' noise level to control the number of errors in the data.
#'
#' @seealso \code{\link{gau_bor_ln}}, \code{\link{sigb_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name gaum_bor_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname gaum_bor_ln
#' @importFrom "stats" "as.formula" "dnorm"
gaum_bor_ln.default <- function(x, y, level, mean = c(0,2), sd = c(sqrt(0.5), sqrt(0.5)), w = c(0.5,0.5), k = 1, sortid = TRUE, ...){

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
  if(any(sd < 0)){
    stop("argument \"sd\" must be higher than 0")
  }
  ncomp <- length(mean)
  if(ncomp != length(sd) || ncomp != length(w)){
    stop("arguments \"mean\", \"sd\" and \"weight\" must be equal to length")
  }
  if(k <= 0){
    stop("argument \"k\" must be higher than 0")
  }
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  
  # min distance
  mindist <- bord_dist(x = x, y = y)

  # density function
  denfun <- rep(0, nrow(x))
  for(i in 1:nrow(x)){
    for(c in 1:ncomp){
      denfun[i] <- denfun[i] + w[c]*dnorm(x = mindist[i], mean = mean[c], sd = sd[c])
    }
  }

  # get noisy samples
  num_noise <- round(nrow(x)*level)

  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE, prob = denfun)
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
  call[[1]] <- as.name("gaum_bor_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Gaussian-mixture borderline label noise",
              param = list(level = level, mean = mean, sd = sd, w = w, k = k, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname gaum_bor_ln
#' @importFrom "stats" "model.frame"
gaum_bor_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- gaum_bor_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("gaum_bor_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
