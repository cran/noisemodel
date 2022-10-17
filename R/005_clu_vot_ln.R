###############################################################
###############################################################
###############################################################

#' @export
clu_vot_ln <- function(x, ...) UseMethod("clu_vot_ln")

#' Clustering-based voting label noise
#'
#' Introduction of \emph{Clustering-based voting label noise} into a classification dataset.
#'
#' \emph{Clustering-based voting label noise} divides the dataset into \code{k} clusters.
#' Then, the labels of each cluster are relabeled with the majority class among its samples.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param k an integer with the number of clusters (default: \code{nlevels(y)}).
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
#' outdef <- clu_vot_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)])
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- clu_vot_ln(formula = Species ~ ., data = iris2D)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References, which considers \emph{k}-means as 
#' unsupervised clustering method.
#'
#' @seealso \code{\link{sco_con_ln}}, \code{\link{mis_pre_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name clu_vot_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname clu_vot_ln
#' @importFrom "stats" "kmeans"
clu_vot_ln.default <- function(x, y, k = nlevels(y), sortid = TRUE, ...){

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
  if(any(sapply(x, is.numeric) == FALSE)){
    stop("column types of \"x\" must be numeric")
  }

  ######################################################
  # introduce noise #########
  yori <- y
    
  clustid <- kmeans(x = x, centers = k)$cluster
  
  for(i in 1:k){
    idc <- which(clustid == i)
    y[idc] <- names(which.max(table(y[idc])))
  }
  
  idx_noise <- which(yori != y)
  num_noise <- length(idx_noise)

  classes <- levels(yori)
  nnoiseclass <- as.vector(table(factor(yori[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(yori, levels = classes)))
  names(distr) <- classes

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("clu_vot_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Clustering-based voting label noise",
              param = list(k = k, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname clu_vot_ln
#' @importFrom "stats" "model.frame"
clu_vot_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- clu_vot_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("clu_vot_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
