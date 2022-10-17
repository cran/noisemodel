###############################################################
###############################################################
###############################################################

#' @export
sym_hie_ln <- function(x, ...) UseMethod("sym_hie_ln")

#' Symmetric hierarchical label noise
#'
#' Introduction of \emph{Symmetric hierarchical label noise} into a classification dataset.
#'
#' \emph{Symmetric hierarchical label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. Then, the labels of these samples are randomly
#' replaced by other ones within the set of class labels related to them (given by the 
#' argument \code{group}). The indices in \code{group} are taken according to the order given by \code{order}. 
#' Note that if a class does not belong to any superclass, it may be mislabeled as any other class.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param group a list of integer vectors with the indices of classes in each superclass.
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
#' D. Hendrycks, M. Mazeika, D. Wilson, and K. Gimpel. \strong{Using trusted 
#' data to train deep networks on labels corrupted by severe noise}. In \emph{Advances in 
#' Neural Information Processing Systems}, volume 31, pages 10477-10486, 2018.
#' url:\url{https://proceedings.neurips.cc/paper/2018/hash/ad554d8c3b06d6b97ee76a2448bd7913-Abstract.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method: a superclass with labels of indices 1 and 2
#' set.seed(9)
#' outdef <- sym_hie_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1, 
#'                        group = list(c(1,2)), order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_hie_ln(formula = Species ~ ., data = iris2D, level = 0.1,
#'                        group = list(c(1,2)), order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_uni_ln}}, \code{\link{sym_def_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_hie_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_hie_ln
sym_hie_ln.default <- function(x, y, level, group, order = levels(y), sortid = TRUE, ...){

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
    for(i in 1:num_noise){
      cl <- as.integer(y[idx_noise[i]])
      ingroup <- sapply(X = group, FUN = function(e){cl %in% e})
      
      if(any(ingroup)){
        noisecla <- group[[which(ingroup)]]
        newvalue <- sample_replace(x = noisecla, size = 1, original = FALSE, ref = cl)
      }
      else{
        newvalue <- sample_replace(x = 1:nlevels(y), size = 1, original = FALSE, ref = cl)
      }

      y[idx_noise[i]] <- order[newvalue]
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_hie_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Symmetric hierarchical label noise",
              param = list(level = level, group = group, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_hie_ln
#' @importFrom "stats" "model.frame"
sym_hie_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_hie_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_hie_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
