###############################################################
###############################################################
###############################################################

#' @export
sym_ddef_ln <- function(x, ...) UseMethod("sym_ddef_ln")

#' Symmetric double-default label noise
#'
#' Introduction of \emph{Symmetric double-default label noise} into a classification dataset.
#'
#' \emph{Symmetric double-default label noise} randomly selects (\code{level}·100)\% of the samples
#' in the dataset with independence of their class. Then, the labels of these samples are
#' replaced by one of two fixed labels (\code{def1} or \code{def2}) within the set of class labels. The indices 
#' \code{def1} and \code{def2} are taken according to the order given by \code{order}.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param def1 an integer with the index of the first default class (default: 1).
#' @param def2 an integer with the index of the second default class (default: 2).
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
#' B. Han, J. Yao, G. Niu, M. Zhou, I. W. Tsang, Y. Zhang, and M. Sugiyama. 
#' \strong{Masking: A new perspective of noisy supervision}. 
#' In \emph{Advances in Neural Information Processing Systems}, volume 31, pages 5841-5851, 2018.
#' url:\url{https://proceedings.neurips.cc/paper/2018/hash/aee92f16efd522b9326c25cc3237ac15-Abstract.html}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_ddef_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)],
#'                       level = 0.1, order = c("virginica", "setosa", "versicolor"))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_ddef_ln(formula = Species ~ ., data = iris2D,
#'                       level = 0.1, order = c("virginica", "setosa", "versicolor"))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_exc_ln}}, \code{\link{sym_cuni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_ddef_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_ddef_ln
sym_ddef_ln.default <- function(x, y, level, def1 = 1, def2 = 2, order = levels(y), sortid = TRUE, ...){

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
  if(def1 < 0 || def1 > nlevels(y)){
    stop("argument \"def1\" must be in [0,nlevels(y)]")
  }
  if(def2 < 0 || def2 > nlevels(y)){
    stop("argument \"def2\" must be in [0,nlevels(y)]")
  }
  if(def1 == def2){
    stop("arguments \"def1\" and \"def2\" must be different")
  }
  if(!all(order %in% levels(y)) || length(order) != nlevels(y)){
    stop("the elements and legnth of \"order\" must match those of levels(y)")
  }

  ######################################################
  # introduce noise #########
  y <- factor(y, levels = order)
  
  xori <- x
  yori <- y
  
  idsel <- 1:nrow(x)
  num_noise <- round(length(idsel)*level)
  idx_noise <- sample(x = idsel, size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- order
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes
  
  if(num_noise > 0){
    for(i in 1:num_noise){
      s <- sample(x = c(TRUE, FALSE), size = 1, replace = FALSE)
      if(s)
        y[idx_noise[i]] <- order[def1]
      else
        y[idx_noise[i]] <- order[def2]
    }
  }

  raux <- findnoise(xori, yori, x, y, "sym_ddef_ln")
  
  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_ddef_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = raux$numnoise,
              idnoise = raux$idnoise,
              numclean = raux$numclean,
              idclean = raux$idclean,
              distr = distr,
              model = "Symmetric double-default label noise",
              param = list(level = level, def1 = def1, def2 = def2, order = order, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_ddef_ln
#' @importFrom "stats" "model.frame"
sym_ddef_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_ddef_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_ddef_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
