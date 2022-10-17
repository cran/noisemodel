###############################################################
###############################################################
###############################################################

#' @export
uncs_guni_cn <- function(x, ...) UseMethod("uncs_guni_cn")

#' Unconditional/symmetric Gaussian/uniform combined noise
#'
#' Introduction of \emph{Unconditional/symmetric Gaussian/uniform combined noise} into a classification dataset.
#'
#' \emph{Unconditional/symmetric Gaussian/uniform combined noise} corrupts all the samples for
#' each attribute in the dataset. Their values are corrupted by adding a random value
#' following a Gaussian distribution of \emph{mean} = 0 and \emph{standard deviation} = (\emph{max}-\emph{min})·\code{k}, being
#' \emph{max} and \emph{min} the limits of the attribute domain. For nominal attributes, a random value is chosen. 
#' Additionally, this noise model also selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. The labels of these samples are randomly
#' replaced by different ones within the set of class labels.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param k a double in [0,1] with the scale used for the standard deviation (default: 0.2).
#' @param sortid a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).
#' @param formula a formula with the output class and, at least, one input attribute.
#' @param data a data frame in which to interpret the variables in the formula.
#' @param ... other options to pass to the function.
#'
#' @return An object of class \code{ndmodel} with elements:
#' \item{xnoise}{a data frame with the noisy input attributes.}
#' \item{ynoise}{a factor vector with the noisy output class.}
#' \item{numnoise}{an integer vector with the amount of noisy samples per variable.}
#' \item{idnoise}{an integer vector list with the indices of noisy samples per variable.}
#' \item{numclean}{an integer vector with the amount of clean samples per variable.}
#' \item{idclean}{an integer vector list with the indices of clean samples per variable.}
#' \item{distr}{an integer vector with the samples per class in the original data.}
#' \item{model}{the full name of the noise introduction model used.}
#' \item{param}{a list of the argument values.}
#' \item{call}{the function call.}
#'
#' @references
#' S. Kazmierczak and J. Mandziuk. 
#' \strong{A committee of convolutional neural networks for image classification in the 
#' concurrent presence of feature and label noise}. 
#' In \emph{Proc. 16th International Conference on Parallel Problem Solving from Nature}, 
#' volume 12269 of LNCS, pages 498-511, 2020.
#' \doi{10.1007/978-3-030-58112-1_34}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # usage of the default method
#' set.seed(9)
#' outdef <- uncs_guni_cn(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#' 
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#' 
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- uncs_guni_cn(formula = Species ~ ., data = iris2D, level = 0.1)
#' 
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_cuni_cn}}, \code{\link{sym_cuni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name uncs_guni_cn
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname uncs_guni_cn
#' @importFrom "stats" "rnorm"
uncs_guni_cn.default <- function(x, y, level, k = 0.2, sortid = TRUE, ...){

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
  if(k < 0 || k > 1){
    stop("argument \"k\" must be in [0,1]")
  }

  ######################################################
  # introduce attribute noise #########
  xori <- x
  yori <- y
  
  num_noise <- 0
  idx_noise <- list()
  idx_clean <- list()
  
  if(level > 0){
    num_noise <- nrow(x)
    for(a in 1:ncol(x)){
      idx_noise[[a]] <- 1:nrow(x)
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
      
      if(is.numeric(x[,a])){
        newvalues <- x[idx_noise[[a]],a] + rnorm(n = num_noise, mean = 0, sd = (max(x[,a])-min(x[,a]))*k)
        if(is.integer(x[,a])){
          newvalues <- round(newvalues)
        }
        newvalues[newvalues < min(x[,a])] <- min(x[,a])
        newvalues[newvalues > max(x[,a])] <- max(x[,a])
      }
      else if(is.factor(x[,a])){
        newvalues <- rep(NA, length(idx_noise[[a]]))
        for(s in 1:length(idx_noise[[a]])){
          newvalues[s] <- sample_replace(x = 1:nlevels(x[,a]), size = 1, original = FALSE, ref = as.integer(x[idx_noise[[a]][s],a]))
        }
        newvalues <- levels(x[,a])[newvalues]
      }
      
      x[idx_noise[[a]],a] <- newvalues
    }
  }
  else{
    for(a in 1:ncol(x)){
      idx_noise[[a]] <- integer(0)
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
    }
  }

  classes <- levels(y)
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  # introduce label noise #########
  if(level > 0){
    num_noise <- round(nrow(x)*level)
    idx_noise[[ncol(x)+1]] <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
    if(sortid)
      idx_noise[[ncol(x)+1]] <- sort(idx_noise[[ncol(x)+1]])
    idx_clean[[ncol(x)+1]] <- setdiff(1:nrow(x),idx_noise[[ncol(x)+1]])
    if(num_noise > 0){
      newvalues <- sample_replace(x = 1:nlevels(y), size = num_noise, original = FALSE, ref = as.integer(y[idx_noise[[ncol(x)+1]]]))
      newvalues <- levels(y)[newvalues]
      y[idx_noise[[ncol(x)+1]]] <- newvalues
    }
    
  }
  
  raux <- findnoise(xori, yori, x, y, "uncs_guni_cn")
  
  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("uncs_guni_cn")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = raux$numnoise,
              idnoise = raux$idnoise,
              numclean = raux$numclean,
              idclean = raux$idclean,
              distr = distr,
              model = "Unconditional/symmetric Gaussian/uniform combined noise",
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
#' @rdname uncs_guni_cn
#' @importFrom "stats" "model.frame"
uncs_guni_cn.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- uncs_guni_cn.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("uncs_guni_cn")

  return(res)
}

###############################################################
###############################################################
###############################################################
