###############################################################
###############################################################
###############################################################

#' @export
imp_int_an <- function(x, ...) UseMethod("imp_int_an")

#' Importance interval-based attribute noise
#'
#' Introduction of \emph{Importance interval-based attribute noise} into a classification dataset.
#'
#' The values in \code{level} are ordered and assigned to attributes according to their information gain (using the 
#' ordering given by \code{ascending}). Then,
#' \emph{Importance interval-based attribute noise} corrupts (\code{level}[i]·100)\% of the values for
#' each attribute \emph{A}[i] in the dataset. In order to corrupt each attribute \emph{A}[i], (\code{level}[i]·100)\% of the
#' samples in the dataset are chosen. To corrupt a value in numeric
#' attributes, the attribute is split into equal-frequency intervals, one of its closest
#' intervals is picked out and a random value within the interval
#' is chosen as noisy. For nominal attributes, a random value within the domain is chosen.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param level a double vector with the noise levels in [0,1] to be introduced into each attribute.
#' @param nbins an integer with the number of bins to create (default: 10).
#' @param ascending a boolean indicating how noise levels are assigned to attributes:
#' \itemize{
#'   \item{\code{TRUE}}{: the lowest noise level is assigned to the most important attribute (default value).}
#'   \item{\code{FALSE}}{: the highest noise level is assigned to the most important attribute.}
#' }
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
#' M. V. Mannino, Y. Yang, and Y. Ryu. 
#' \strong{Classification algorithm sensitivity to training data with non representative attribute noise}. 
#' \emph{Decision Support Systems}, 46(3):743-751, 2009.
#' \doi{10.1016/j.dss.2008.11.021}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- imp_int_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
#'                         level = c(0.1, 0.2))
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- imp_int_an(formula = Species ~ ., data = iris2D, 
#'                         level = c(0.1, 0.2))
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{asy_int_an}}, \code{\link{asy_uni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name imp_int_an
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname imp_int_an
#' @importFrom "stats" "runif"
imp_int_an.default <- function(x, y, level, nbins = 10, ascending = TRUE, sortid = TRUE, ...){

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
  if(length(level) != ncol(x)){
    stop("length of \"level\" must be equal to the number of columns in \"x\"")
  }
  if(nbins <= 1){
    stop("argument \"nbins\" must be higher than 1")
  }

  ######################################################
  # introduce noise #########
  imp <- rep(NA, ncol(x))
  for(a in 1:ncol(x)){
    imp[a] <- infogain(x = x, y = y, att = a)
  }
  imp <- sort(x = imp, decreasing = TRUE, index.return = TRUE)$ix
  level <- level[sort(x = level, decreasing = !ascending, index.return = TRUE)$ix]

  idsort <- sort(x = imp, decreasing = FALSE, index.return = TRUE)$ix
  level <- level[idsort]

  num_noise <- round(level*nrow(x))

  idx_noise <- list()
  idx_clean <- list()

  for(a in 1:ncol(x)){

    if(num_noise[a] > 0){

      idx_noise[[a]] <- sample(1:nrow(x), num_noise[a], replace = FALSE)
      if(sortid){
        idx_noise[[a]] <- sort(idx_noise[[a]])
      }
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])

      if(is.numeric(x[,a])){

        disc <- classIntervals(x[,a], nbins, style = 'quantile')
        breaks_ <- unique(disc$brks)
        realbins_ <- length(breaks_)-1

        min.int <- breaks_[1:(length(breaks_)-1)]
        max.int <- breaks_[2:length(breaks_)]

        part_j <- cut(x[,a], breaks = breaks_, labels = FALSE, include.lowest = TRUE, right = FALSE)

        for(sindx in 1:length(idx_noise[[a]])){
          s <- idx_noise[[a]][sindx]

          if(part_j[s] == 1)
            noiseint <- 2
          else if(part_j[s] == realbins_)
            noiseint <- realbins_ - 1
          else
            noiseint <- sample(x = c(part_j[s]-1, part_j[s]+1), size = 1)

          x[s,a] <- runif(n = 1, min = min.int[noiseint], max = max.int[noiseint])
        }

        if(is.integer(x[,a]))
          x[s,a] <- round(x[s,a])
      }

      else if(is.factor(x[,a])){
        newvalues <- sample_replace(x = 1:nlevels(x[,a]), size = num_noise[a], original = FALSE, ref = as.integer(x[idx_noise[[a]],a]))
        newvalues <- levels(x[,a])[newvalues]
        x[idx_noise[[a]],a] <- newvalues
      }

    }

    else{
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
  call[[1]] <- as.name("imp_int_an")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = num_noise,
              idnoise = idx_noise,
              numclean = nrow(x)-num_noise,
              idclean = idx_clean,
              distr = distr,
              model = "Importance interval-based attribute noise",
              param = list(level = level, nbins = nbins, ascending = ascending, sortid = sortid),
              call = call
  )
  class(res) <- "ndmodel"
  return(res)
}

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname imp_int_an
#' @importFrom "stats" "model.frame"
imp_int_an.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- imp_int_an.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("imp_int_an")

  return(res)
}

###############################################################
###############################################################
###############################################################

infogain <- function(x, y, att = 1, bins = 5, ...){
  
  ######################################################
  # metric computation #########
  ent.class <- infoentropy(y)
  nsamples <- nrow(x)
  
  if(!is.factor(x[,att]))
    disc <- cut(x = x[,att], breaks = bins, labels = c(1:bins))
  else{
    disc <- as.integer(x[,att])
    bins <- nlevels(x[,att])
  }
  
  aux <- rep(-1, bins)
  for(i in 1:bins){
    values <- y[disc == i]
    aux[i] <- (length(values)/nsamples)*infoentropy(values)
  }
  aux <- sum(aux)
  res <- ent.class - aux
  return(res)
  
}

###############################################################
###############################################################
###############################################################

# information entropy (att is a factor vector containing the attribute)
infoentropy <- function(att){
  
  freq <- table(att)/length(att)
  freq <- as.data.frame(freq)[,2]
  freq <- freq[freq > 0]
  
  # compute entropy
  res <- -sum(freq*log2(freq))
  return(res)
}

###############################################################
###############################################################
###############################################################
