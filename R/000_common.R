###############################################################
###############################################################
###############################################################

#' Type of noise introduced by a noise model
#'
#' Given the function name of a model, it returns the type of noise it 
#' introduces: label, attributes, or both.
#'
#' @param model a character with the function name of the noise model.
#'
#' @return A character with the type of noise \code{model} introduces. It can be \code{cla} for 
#' label noise, \code{att} for attribute noise or \code{com} for combined noise.
#'
#' @export
#' @keywords internal
noisetype <- function(model){
  
  type <- ""
  
  if(model == "sym_uni_an" || model == "sym_cuni_an" || 
     model == "sym_end_an" || 
     model == "salt_unif_an" || model == "sym_gau_an" || model == "unc_vgau_an" ||
     model == "sym_sgau_an" || model == "sym_pgau_an" || model == "asy_int_an" ||
     model == "imp_int_an" || model == "sym_int_an" || model == "symd_uni_an" ||
     model == "symd_gau_an" || model == "boud_gau_an" || model == "asy_uni_an" ||
     model == "unc_fixw_an" || model == "symd_rpix_an" || model == "symd_gimg_an"
     
  ){
    type <- "att"
  }
  else if(model == "sym_cuni_cn" || model == "uncs_guni_cn"){
    type <- "com"
  }
  else
    type <- "cla"
  
  return(type)
}

###############################################################
###############################################################
###############################################################

#' Find the differences between two datasets
#'
#' Detect the differences between two datasets, focusing on the input attributes (\code{x}, 
#' \code{xnoise}), the output class (\code{y}, \code{ynoise}) or both depending on the type of 
#' the model (label, attributes, combined).
#'
#' @param x a data frame of input attributes (clean dataset).
#' @param y a factor vector with the output class of each sample (clean dataset).
#' @param xnoise a data frame of input attributes (noisy dataset).
#' @param ynoise a factor vector with the output class of each sample (noisy dataset).
#' @param model a character with the name of the noise model.
#'
#' @return A list with four elements:
#' \item{numnoise}{an integer vector with the amount of noisy samples per variable.}
#' \item{idnoise}{an integer vector list with the indices of noisy samples per variable.}
#' \item{numclean}{an integer vector with the amount of clean samples per variable.}
#' \item{idclean}{an integer vector list with the indices of clean samples per variable.}
#'
#' @export
#' @keywords internal
findnoise <- function(x, y, xnoise, ynoise, model){
  
  if(noisetype(model) == "com"){
    
    numnoise <- rep(0, ncol(x)+1)
    numclean <- rep(0, ncol(x)+1)
    idx_noise <- list()
    idx_clean <- list()
    
    for(a in 1:ncol(x)){
      idx_noise[[a]] <- which(x[,a] != xnoise[,a])
      numnoise[a] <- length(idx_noise[[a]])
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
      numclean[a] <- length(idx_clean[[a]])
    }
    
    a <- ncol(x)+1
    idx_noise[[a]] <- which(y != ynoise)
    numnoise[a] <- length(idx_noise[[a]])
    idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
    numclean[a] <- length(idx_clean[[a]])
  }
  
  if(noisetype(model) == "att"){
    
    numnoise <- rep(0, ncol(x))
    numclean <- rep(0, ncol(x))
    idx_noise <- list()
    idx_clean <- list()
    
    for(a in 1:ncol(x)){
      idx_noise[[a]] <- which(x[,a] != xnoise[,a])
      numnoise[a] <- length(idx_noise[[a]])
      idx_clean[[a]] <- setdiff(1:nrow(x),idx_noise[[a]])
      numclean[a] <- length(idx_clean[[a]])
    }
    
  }
  
  if(noisetype(model) == "cla"){
    
    idx_noise <- which(y != ynoise)
    idx_clean <- setdiff(1:nrow(x),idx_noise)
    
    classes <- levels(y)
    
    numnoise <- as.vector(table(factor(y[idx_noise], levels = classes)))
    names(numnoise) <- classes
    
    distr <- as.vector(table(factor(y, levels = classes)))
    names(distr) <- classes
    
    numclean <- distr - numnoise
    
    idx_noise <- list(idx_noise)
    idx_clean <- list(idx_clean)
  }
  
  res <- list(numnoise = numnoise,
              idnoise = idx_noise,
              numclean = numclean,
              idclean = idx_clean
  )
  
  return(res)
}

###############################################################
###############################################################
###############################################################

#' Safe sample function
#'
#' Similar to standard \code{sample} function. Safe sample function considering the special case of an integer vector with only one element.
#'
#' @param x a vector with the alternatives to choose.
#' @param size an integer with the number of elements to select from \code{x}.
#' @param replace a boolean indicating if the elements should be chosen with replacement (default: \code{FALSE}).
#' @param prob a double vector with the probability associated to each element (default: \code{NULL}).
#'
#' @return A vector with the elements chosen.
#'
#' @export
#' @keywords internal
safe_sample <- function(x, size, replace = FALSE, prob = NULL){
  
  if(length(x) == 1){
    res <- x
  }
  else{
    res <- sample(x = x, size = size, replace = replace, prob = prob)
  }
  
  return(res)
}

###############################################################
###############################################################
###############################################################

#' Sample considering reference values
#'
#' Similar to standard \code{sample} function. The values in \code{ref} can be chosen or not, 
#' according to \code{original}.
#'
#' @param x a vector with the alternatives to choose.
#' @param size an integer with the number of elements to select from \code{x}.
#' @param original a boolean indicating if the values in \code{ref} can be chosen.
#' @param ref a vector with \code{n} reference values.
#'
#' @return A vector with the elements chosen from \code{x}.
#'
#' @export
#' @keywords internal
#' @importFrom "stats" "runif"
sample_replace <- function(x, size, original, ref){
  
  newvalues <- rep(NA, size)
  
  if(original){
    newvalues <- sample(x = x, size = size, replace = TRUE)
  }
  else{
    for(i in 1:size){
      correct <- FALSE
      while(!correct){
        newvalues[i] <- sample(x = x, size = 1)
        if(newvalues[i] != ref[i]){
          correct <- TRUE
        }
      }
    }
  }
  
  return(newvalues)
}

###############################################################
###############################################################
###############################################################

#' Random numbers considering reference values
#'
#' Generate \code{n} random numbers following a uniform distribution 
#' between \code{min} and \code{max}. The values in \code{ref} can be chosen or not, 
#' according to \code{original}.
#'
#' @param n an integer with the amount of random numbers to generate.
#' @param min a double with the lower limit of the distribution.
#' @param max a double with the upper limit of the distribution.
#' @param original a boolean indicating if the values in \code{ref} can be chosen.
#' @param ref a double vector with \code{n} reference values.
#'
#' @return A double vector with the numbers generated.
#'
#' @export
#' @keywords internal
#' @importFrom "stats" "runif"
runif_replace <- function(n, min, max, original, ref){
  
  newvalues <- rep(NA, n)
  
  if(original){
    newvalues <- runif(n = n, min = min, max = max)
  }
  else{
    for(i in 1:n){
      correct <- FALSE
      while(!correct){
        newvalues[i] <- runif(n = 1, min = min, max = max)
        if(newvalues[i] != ref[i]){
          correct <- TRUE
        }
      }
    }
  }
  
  return(newvalues)
}

###############################################################
###############################################################
###############################################################

#' Distance to SVM decision boundary
#'
#' Calculation of the distance of each sample to the SVM decision boundary in a classification problem.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param krn a character with the kernel of SVM -see \code{e1071::svm} (default: "\code{linear}").
#'
#' @return A vector of length \code{nrow(x)} with the distance of each sample to the decision boundary.
#'
#' @export
#' @keywords internal
#' @importFrom "e1071" "svm"
#' @importFrom "stringr" "str_detect"
bord_dist <- function(x, y, krn = "linear"){
  
  if(any(sapply(x,is.factor))){
    x <- expandFactors(x)
  }
  
  model <- svm(x = x, y = y, type = "C-classification", kernel = krn, decision.values = TRUE)
  decvalues <- predict(model, x, decision.values = TRUE)
  decvalues <- attr(decvalues, "decision.values")
  decvalues <- abs(decvalues)
  
  for(i in 1:nrow(x)){
    l <- as.character(y[i])
    cn <- colnames(decvalues)
    decvalues[i,!str_detect(cn, l)] <- Inf 
  }
  
  dist <- apply(X = decvalues, MARGIN = 1, FUN = function(p){min(p)})
  
  return(dist)
}

###############################################################
###############################################################
###############################################################

#' Mislabeling based on k-nearest neighbors
#'
#' Computation of a noisy label based on majority class among \emph{k} nearest neighbors with different label.
#'
#' @param x a data frame of input attributes.
#' @param y a factor vector with the output class of each sample.
#' @param num_noise an integer with the number of noisy samples.
#' @param idx_noise an integer vector with the indices of noisy samples.
#' @param k an integer with the number of nearest neighbors to use.
#'
#' @return A vector of length \code{length(y)} with the class of each sample, including the new noisy 
#' classes for the samples with indices \code{idx_noise}.
#'
#' @export
#' @keywords internal
#' @importFrom "FNN" "get.knnx"
bord_noise <- function(x, y, num_noise, idx_noise, k){
  
  classes <- levels(y)
  
  idx <- list()
  for(c in 1:length(classes)){
    idx[[c]] <- which(y == classes[c])
  }
  
  newclasses <- rep(NA, num_noise)
  for(i in 1:num_noise){
    class <- as.integer(y[idx_noise[i]])
    id_sameclass <- setdiff(idx[[class]],idx_noise[i])
    id_difclass <- setdiff(1:nrow(x), idx[[class]])
    
    kmin <- min(k, length(id_difclass))
    nn <- get.knnx(data = x[id_difclass,], query = x[idx_noise[i],], k = kmin, algorithm = "brute")$nn.index
    nn_cla <- y[id_difclass][nn]
    majcla <- unname(which.max(table(nn_cla)))
    
    newclasses[i] <- majcla
  }
  
  newclasses <- levels(y)[newclasses]
  y[idx_noise] <- newclasses
  
  return(y)
}

###############################################################
###############################################################
###############################################################
