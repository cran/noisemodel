###############################################################
###############################################################
###############################################################

#' @export
sym_con_ln <- function(x, ...) UseMethod("sym_con_ln")

#' Symmetric confusion label noise
#'
#' Introduction of \emph{Symmetric confusion label noise} into a classification dataset.
#'
#' \emph{Symmetric confusion label noise} considers that the mislabeling probability for each 
#' class is \code{level}. It obtains the confusion matrix from the dataset, which is 
#' row-normalized to estimate the transition matrix and get the probability of selecting each class 
#' when noise occurs.
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
#' D. Ortego, E. Arazo, P. Albert, N. E. O’Connor, and K. McGuinness. 
#' \strong{Towards robust learning with different label noise distributions}. 
#' In \emph{Proc. 25th International Conference on Pattern Recognition}, pages 7020-7027, 2020.
#' \doi{10.1109/ICPR48806.2021.9412747}.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#'
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_con_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#'
#' # show results
#' summary(outdef, showid = TRUE)
#' plot(outdef)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' outfrm <- sym_con_ln(formula = Species ~ ., data = iris2D, level = 0.1)
#'
#' # check the match of noisy indices
#' identical(outdef$idnoise, outfrm$idnoise)
#' 
#' @note Noise model adapted from the papers in References, considering C5.0 as classifier.
#'
#' @seealso \code{\link{sym_cen_ln}}, \code{\link{glev_uni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#'
#' @name sym_con_ln
NULL

###############################################################
###############################################################
###############################################################

#' @export
#' @rdname sym_con_ln
#' @importFrom "caret" "confusionMatrix" "train" "trainControl"
#' @importFrom "C50" "C5.0"
sym_con_ln.default <- function(x, y, level, sortid = TRUE, ...){

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
  grid <- expand.grid( .winnow = c(TRUE), .trials=c(1), .model="tree" )
  model <- train(x, y, method = "C5.0", trControl = trainControl(method = "none"), tuneGrid = grid)
  pred <- predict(model, newdata = x)
  conf <- unname(t(confusionMatrix(data = pred, reference = y)$table))
  tramat <- apply(conf, 1, function(x){x/sum(x)})
  
  tramat2 <- tramat
  diag(tramat2) <- 0
  sc <- unname(rowSums(tramat2))
  
  for(i in 1:ncol(tramat2)){
    if(length(unique(tramat2[i,-i])) == 1)
      nl <- rep(level/(ncol(tramat2)-1),ncol(tramat2)-1)
    else{
      v <- tramat2[i,-i]
      v <- v/sum(v)
      nl <- v*level
    }
    
    tramat2[i,-i] <- nl
  }
  
  # select noisy samples
  num_noise <- round(nrow(x)*level)
  idx_noise <- sample(x = 1:nrow(x), size = num_noise, replace = FALSE)
  if(sortid)
    idx_noise <- sort(idx_noise)
  
  classes <- levels(y)
  nnoiseclass <- as.vector(table(factor(y[idx_noise], levels = classes)))
  names(nnoiseclass) <- classes
  distr <- as.vector(table(factor(y, levels = classes)))
  names(distr) <- classes

  # select noisy values
  if(num_noise > 0){
    for(s in 1:num_noise){
      y[idx_noise[s]] <- sample(x = levels(y), size = 1, prob = tramat2[y[idx_noise[s]],])
    }
  }
  
  ######################################################
  # create object of class 'ndmodel' #########
  call <- match.call()
  call[[1]] <- as.name("sym_con_ln")
  res <- list(xnoise = x,
              ynoise = y,
              numnoise = nnoiseclass,
              idnoise = list(idx_noise),
              numclean = distr-nnoiseclass,
              idclean = list(setdiff(1:nrow(x),idx_noise)),
              distr = distr,
              model = "Symmetric confusion label noise",
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
#' @rdname sym_con_ln
#' @importFrom "stats" "model.frame"
sym_con_ln.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- sym_con_ln.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("sym_con_ln")

  return(res)
}

###############################################################
###############################################################
###############################################################
