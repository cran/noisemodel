###############################################################
###############################################################
###############################################################

#' @export
sco_con_ln <- function(x, ...) UseMethod("sco_con_ln")

#' Score-based confidence label noise
#'
#' Introduction of \emph{Score-based confidence label noise} into a classification dataset.
#'
#' \emph{Score-based confidence label noise} follows the intuition that hard samples are 
#' more likely to be mislabeled. Given the confidence per class of each sample, 
#' if it is predicted with a different class with a high probability, it means that 
#' it is hard to clearly distinguish the sample from this class. The confidence information is used to compute a mislabeling score for each sample and its potential noisy 
#' label. Finally, (\code{level}·100)\% of the samples with the highest mislabeling scores 
#' are chosen as noisy.
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
#' P. Chen, J. Ye, G. Chen, J. Zhao, and P. Heng. 
#' \strong{Beyond class-conditional assumption: A primary attempt to combat instance-dependent label noise}. 
#' In \emph{Proc. 35th AAAI Conference on Artificial Intelligence}, pages 11442-11450, 2021.
#' url:\url{https://ojs.aaai.org/index.php/AAAI/article/view/17363}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sco_con_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sco_con_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References.
#'
#' @seealso \code{\link{mis_pre_ln}}, \code{\link{smam_bor_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sco_con_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sco_con_ln
#' @importFrom "lsr" "expandFactors"
#' @importFrom "nnet" "class.ind"
#' @importFrom "nnet" "nnet"
sco_con_ln.default <- function(x, y, level, sortid = TRUE, ...){

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

  ######################################################
  # introduce noise #########
  nit <- 100
  x2 <- expandFactors(x)
  ideal <- class.ind(y)
  
  resnn <- array(data = NA, dim = c(nit, nrow(x2), ncol(ideal)))
  cseed <- sample(x = 1:1000000, size = 1)
  for(i in 1:nit){
    set.seed(cseed)
    model <- nnet(x2, ideal, size = 10, softmax = TRUE, maxit = i, trace = FALSE)
    resnn[i,,] <- model$fitted.values
  }
  
  # compute mean
  S <- array(data = 0, dim = c(nrow(x2), ncol(ideal)))
  for(i in 1:nit){
    S <- S + resnn[i,,]
  }
  S <- S/nit
  
  # compute noise score and noisy label for each sample
  N <- rep(0, nrow(x))
  ynoise <- rep(-1, nrow(x))
  for(i in 1:nrow(x)){
    c <- as.integer(y[i])
    v <- S[i,]
    v[c] <- -Inf
    N[i] <- max(v)
    ynoise[i] <- which.max(v)
  }
  
  # select high noise scores
  id <- sort(x = N, decreasing = TRUE, index.return = TRUE)$ix

  num_noise <- round(nrow(x)*level)
  if(num_noise > 0){
    idx_noise <- id[1:num_noise]
    if(sortid)
      idx_noise <- sort(idx_noise)
  }
  else{
    idx_noise <- NULL
  }
  
  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  # assign noise labels
  if(num_noise > 0){
    
    for(i in 1:num_noise){
      newcla <- ynoise[idx_noise[i]]
      newcla <- levels(y)[newcla]
      y[idx_noise[i]] <- newcla
    }
  }

  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sco_con_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Score-based confidence label noise",
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
#' @rdname sco_con_ln
#' @importFrom "stats" "model.frame"
sco_con_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sco_con_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sco_con_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
