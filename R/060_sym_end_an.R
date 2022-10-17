###############################################################
###############################################################
###############################################################

#' @export
sym_end_an <- function(x, ...) UseMethod("sym_end_an")

#' Symmetric end-directed attribute noise
#'
#' Introduction of \emph{Symmetric end-directed attribute noise} into a classification dataset.
#'
#' For each attribute \emph{A}, \emph{Symmetric end-directed attribute noise} computes a
#' value \code{k} = \code{scale}·\emph{max}(\emph{A}). Then, it chooses (\code{level}·100)\% of the values of that
#' attribute. For each value, it applies the following procedure:
#' \itemize{
#'    \item If the value is less than the median of the attribute, the value transforms into
#'    adding \code{k} to the maximum of the attribute \emph{A}.
#'    \item If the value is greater than the median of the attribute, the value transforms into 
#'    subtracting \code{k} from the minimum of the attribute \emph{A}.
#'    \item If the value matches the median, one of the two previous alternatives is chosen.
#' }
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double in [0,1] with the noise level to be introduced.
#' @param scale a double in (0,1) with the scale to be used (default: 0.2).
#' @param sortid a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).
#' @param formula a formula with the output class and, at least, one input attribute.
#' @param data a data frame in which to interpret the variables in the formula.
#' @param ... other options to pass to the function.
#'
#' @return An object of class \code{ndmodel} with elements:
#' \item{xnoise}{a data frame with the noisy input attributes.}
#' \item{ynoise}{a factor vector with the noisy output class.}
#' \item{numnoise}{an integer vector with the amount of noisy samples per attribute.}
#' \item{idnoise}{an integer vector list with the indices of noisy samples per attribute.}
#' \item{numclean}{an integer vector with the amount of clean samples per attribute.}
#' \item{idclean}{an integer vector list with the indices of clean samples per attribute.}
#' \item{distr}{an integer vector with the samples per class in the original data.}
#' \item{model}{the full name of the noise introduction model used.}
#' \item{param}{a list of the argument values.}
#' \item{call}{the function call.}
#'
#' @references
#' T. M. Khoshgoftaar and J. V. Hulse. 
#' \strong{Empirical case studies in attribute noise detection}. 
#' \emph{IEEE Transactions on Systems, Man and Cybernetics Part C: Applications and Reviews}, 39(4):379-388, 2009.
#' \doi{10.1109/TSMCC.2009.2013815}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_end_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_end_an(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{sym_sgau_an}}, \code{\link{symd_gau_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_end_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_end_an
#' @importFrom "stats" "rnorm" "median"
sym_end_an.default <- function(x, y, level, scale = 0.2, sortid = TRUE, ...){

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
  if(scale <= 0 || scale >= 1){
    stop("argument \"scale\" must be in (0,1)")
  }
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }

  ######################################################
  # introduce noise #########
  num_noise <- round(level*nrow(x))
  idx_noise <- list()
  idx_clean <- list()

  if(level > 0){

    for(a in 1:ncol(x)){
      idx_noise[[a]] <- sample(1:nrow(x), num_noise, replace = FALSE)
      if(sortid){
        idx_noise[[a]] <- sort(idx_noise[[a]])
      }
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])

      if(is.numeric(x[,a])){

        k <- scale*max(x[,a])

        md <- median(x[,a])
        mx <- max(x[,a])
        mn <- min(x[,a])

        for(s in 1:length(idx_noise[[a]])){

          if(x[idx_noise[[a]][s],a] < md)
            x[idx_noise[[a]][s],a] <- mx + k
          else if(x[idx_noise[[a]][s],a] > md)
            x[idx_noise[[a]][s],a] <- mn - k
          else if(x[idx_noise[[a]][s],a] == md){
            if(sample(x = c(0,1), size = 1) == 0)
              x[idx_noise[[a]][s],a] <- mx + k
            else
              x[idx_noise[[a]][s],a] <- mn - k
          }

          if(is.integer(x[,a]))
            x[idx_noise[[a]][s],a] <- round(x[idx_noise[[a]][s],a])
        }

      }
      else if(is.factor(x[,a])){
        newvalues <- sample_replace(x = 1:nlevels(x[,a]), size = num_noise, original = FALSE, ref = as.integer(x[idx_noise[[a]],a]))
        newvalues <- levels(x[,a])[newvalues]
        x[idx_noise[[a]],a] <- newvalues
      }

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

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_end_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = rep(num_noise,ncol(x)),
              idnoise = idx_noise,
              numclean = rep(nrow(x),ncol(x))-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Symmetric end-directed attribute noise",
              param = list(level = level, scale = scale, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_end_an
#' @importFrom "stats" "model.frame"
sym_end_an.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_end_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_end_an")

  return(res)
}

###############################################################
###############################################################
###############################################################
