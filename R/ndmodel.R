###############################################################
###############################################################
###############################################################

#' Print function for class ndmodel
#'
#' This method displays the basic information about the noise
#' introduction process contained in an object of class \code{ndmodel}.
#'
#' This function presents the basic information of the noise introduction process and the resulting noisy dataset contained in the object \code{x} of class \code{ndmodel}.
#' The information offered is as follows:
#' \itemize{
#'    \item the name of the noise introduction model.
#'    \item the parameters associated with the noise model.
#'    \item the number of noisy and clean samples in the dataset.
#' }
#'
#' @param x an object of class \code{ndmodel}.
#' @param ... other options to pass to the function.
#'
#' @return This function does not return any value.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#' 
#' # show results
#' print(outdef)
#' 
#' @seealso \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}, \code{\link{sym_uni_ln}}, \code{\link{sym_cuni_ln}}, \code{\link{sym_uni_an}}
#' 
#' @export
print.ndmodel <- function(x, ...){

  cat("\n## Noise model: ", x$model, sep="\n")

  if(!is.null(x$param)){
    cat("\n## Parameters:\n")
    for (i in 1:length(x$param)){

      if( (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_adj_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sigb_uni_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_opt_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "ulap_bor_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_ddef_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "ugau_bor_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_def_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "oned_uni_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_hie_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "opes_idnn_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "asy_spl_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "opes_idc_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "pai_mer_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "opes_idu_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "pai_bdir_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "asy_def_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "asy_uni_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "mulc_udir_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_hienc_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_pes_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_dia_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "sym_nexc_ln") ||
          (attr(x$param,"names")[i] == "order" && x$call[[1]] == "asy_spa_ln") ||
          (attr(x$param,"names")[i] == "pairs" && x$call[[1]] == "pai_bdir_ln") ||
          (attr(x$param,"names")[i] == "pairs" && x$call[[1]] == "pai_mer_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "qua_uni_ln") ||
          (attr(x$param,"names")[i] == "goal" && x$call[[1]] == "mulc_udir_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "asy_def_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "asy_spl_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "ugau_bor_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "ulap_bor_ln") ||
          (attr(x$param,"names")[i] == "lower" && x$call[[1]] == "oned_uni_ln") ||
          (attr(x$param,"names")[i] == "upper" && x$call[[1]] == "oned_uni_ln") ||
          (attr(x$param,"names")[i] == "group" && x$call[[1]] == "sym_hienc_ln") ||
          (attr(x$param,"names")[i] == "group" && x$call[[1]] == "sym_hie_ln") ||
          (attr(x$param,"names")[i] == "mean" && x$call[[1]] == "gaum_bor_ln") ||
          (attr(x$param,"names")[i] == "sd" && x$call[[1]] == "gaum_bor_ln") ||
          (attr(x$param,"names")[i] == "w" && x$call[[1]] == "gaum_bor_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "sigb_uni_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "quab_uni_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "asy_uni_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "asy_uni_an") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "imp_int_an") ||
          (attr(x$param,"names")[i] == "tramat" && x$call[[1]] == "sym_nuni_ln") ||
          (attr(x$param,"names")[i] == "level" && x$call[[1]] == "asy_int_an")
          ){
        values <- paste0(x$param[[i]], collapse = ", ")
        cat("- ",attr(x$param,"names")[i]," = ",values,"\n",sep = "")
      }

      else
        cat("- ",attr(x$param,"names")[i]," = ",x$param[[i]],"\n",sep = "")
    }
  }

  if(noisetype(x$call[[1]]) == "att"){

    cat("\n## Number of noisy and clean attribute values:\n")
    ncle <- sum(x$numclean)
    nnoi <- sum(x$numnoise)
    ntot <- ncle+nnoi
    cat("- Noisy values: ", nnoi,"/",ntot," (",round(nnoi*100/ntot,2),"%)","\n",sep="")
    cat("- Clean values: ", ncle,"/",ntot," (",round(ncle*100/ntot,2),"%)","\n",sep="")

  }
  
  else if(noisetype(x$call[[1]]) == "com"){
    cat("\n## Number of noisy and clean values:\n")
    ncle <- sum(x$numclean[-length(x$numclean)])
    nnoi <- sum(x$numnoise[-length(x$numclean)])
    ntot <- ncle+nnoi
    cat("- Noisy attribute values: ", nnoi,"/",ntot," (",format(round(nnoi*100/ntot, 2), nsmall = 2),"%)","\n",sep="")
    cat("- Clean attribute values: ", ncle,"/",ntot," (",format(round(ncle*100/ntot, 2), nsmall = 2),"%)","\n",sep="")
    ncle <- sum(x$numclean[length(x$numclean)])
    nnoi <- sum(x$numnoise[length(x$numnoise)])
    ntot <- ncle+nnoi
    cat("- Noisy label values: ", nnoi,"/",ntot," (",format(round(nnoi*100/ntot, 2), nsmall = 2),"%)","\n",sep="")
    cat("- Clean label values: ", ncle,"/",ntot," (",format(round(ncle*100/ntot, 2), nsmall = 2),"%)","\n",sep="")
  }
  
  else{
    cat("\n## Number of noisy and clean samples:\n")
    ncle <- sum(x$numclean)
    nnoi <- sum(x$numnoise)
    ntot <- ncle+nnoi
    cat("- Noisy samples: ", nnoi,"/",ntot," (",format(round(nnoi*100/ntot, 2), nsmall = 2),"%)","\n",sep="")
    cat("- Clean samples: ", ncle,"/",ntot," (",format(round(ncle*100/ntot, 2), nsmall = 2),"%)","\n",sep="")
  }

}

###############################################################
###############################################################
###############################################################

#' Summary function for class ndmodel
#'
#' This method displays a summary containing information about the noise
#' introduction process contained in an object of class \code{ndmodel}.
#'
#' This function presents a summary containing information of the noise introduction process and the resulting
#' noisy dataset contained in the object \code{object} of class \code{ndmodel}.
#' The information offered is as follows:
#' \itemize{
#'    \item the function call.
#'    \item the name of the noise introduction model.
#'    \item the parameters associated with the noise model.
#'    \item the number of noisy and clean samples in the dataset.
#'    \item the number of noisy samples per class/attribute.
#'    \item the number of clean samples per class/attribute.
#'    \item the indices of the noisy samples (if \code{showid = TRUE}).
#' }
#'
#' @param object an object of class \code{ndmodel}.
#' @param showid a logical indicating if the indices of noisy samples must be displayed (default: \code{FALSE}).
#' @param ... other options to pass to the function.
#'
#' @return A list with the elements of \code{object}, including the \code{showid} argument.
#'
#' @examples
#' # load the dataset
#' data(iris2D)
#' 
#' # usage of the default method
#' set.seed(9)
#' outdef <- sym_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)
#' 
#' # show results
#' summary(outdef, showid = TRUE)
#' 
#' @seealso \code{\link{print.ndmodel}}, \code{\link{plot.ndmodel}}, \code{\link{sym_uni_ln}}, \code{\link{sym_cuni_ln}}, \code{\link{sym_uni_an}}
#' 
#' @export
summary.ndmodel <- function(object, ..., showid = FALSE){

  object <- structure(object, class = "sum.ndmodel")
  object$showid <- showid
  return(object)
}

###############################################################
###############################################################
###############################################################

#' Print function for class sum.ndmodel
#'
#' Auxiliary function for printing information about the noise
#' introduction process contained in an object of class \code{sum.ndmodel}.
#'
#' @param x an object of class \code{sum.ndmodel}.
#' @param ... other options to pass to the function.
#'
#' @return This function does not return any value.
#' 
#' @export
#' @keywords internal
print.sum.ndmodel <- function(x, ...){

  cat("\n########################################################\n")
  cat("\tNoise introduction process: Summary\n")
  cat("########################################################\n\n")

  call <- deparse(x$call, getOption("width"))

  cat("## Original call:", call, sep = "\n")

  print.ndmodel(x)

  if(noisetype(x$call[[1]]) == "cla"){
    cat("\n## Number of noisy samples per class label:\n")
    for(i in 1:length(x$numnoise))
      cat("- Class ", names(x$numnoise)[i], ": ", x$numnoise[i],"/",x$distr[i], " (",format(round(x$numnoise[i]*100/x$distr[i], 2), nsmall = 2),"%)","\n",sep="")
    
    cat("\n## Number of clean samples per class label:\n")
    for(i in 1:length(x$numclean))
      cat("- Class ", names(x$numclean)[i], ": ", x$numclean[i],"/",x$distr[i], " (",format(round(x$numclean[i]*100/x$distr[i], 2), nsmall = 2),"%)","\n",sep="")
  }
  
  else if(noisetype(x$call[[1]]) == "att"){
    cat("\n## Number of noisy samples per attribute:\n")
    for(a in 1:length(x$numnoise)){
      ncle <- sum(x$numclean[a])
      nnoi <- sum(x$numnoise[a])
      ntot <- ncle+nnoi
      cat("- Attribute ", names(x$x)[a], ": ", nnoi,"/",ntot," (",round(nnoi*100/ntot, 2),"%)","\n",sep="")
    }
    
    cat("\n## Number of clean samples per attribute:\n")
    for(a in 1:length(x$numnoise)){
      ncle <- sum(x$numclean[a])
      nnoi <- sum(x$numnoise[a])
      ntot <- ncle+nnoi
      cat("- Attribute ", names(x$x)[a], ": ", ncle,"/",ntot," (",round(ncle*100/ntot, 2),"%)","\n",sep="")
    }
  }
  
  else if(noisetype(x$call[[1]]) == "com"){
    cat("\n## Number of noisy samples per variable:\n")
    for(a in 1:(length(x$numnoise)-1)){
      ncle <- sum(x$numclean[a])
      nnoi <- sum(x$numnoise[a])
      ntot <- ncle+nnoi
      cat("- Attribute ", names(x$x)[a], ": ", nnoi,"/",ntot," (",round(nnoi*100/ntot, 2),"%)","\n",sep="")
    }
    a <- length(x$numnoise)
    ncle <- sum(x$numclean[a])
    nnoi <- sum(x$numnoise[a])
    ntot <- ncle+nnoi
    cat("- Output class: ", nnoi,"/",ntot," (",round(nnoi*100/ntot, 2),"%)","\n",sep="")
    
    
    cat("\n## Number of clean samples per variable:\n")
    for(a in 1:(length(x$numnoise)-1)){
      ncle <- sum(x$numclean[a])
      nnoi <- sum(x$numnoise[a])
      ntot <- ncle+nnoi
      cat("- Attribute ", names(x$x)[a], ": ", ncle,"/",ntot," (",round(ncle*100/ntot, 2),"%)","\n",sep="")
    }
    a <- length(x$numnoise)
    ncle <- sum(x$numclean[a])
    nnoi <- sum(x$numnoise[a])
    ntot <- ncle+nnoi
    cat("- Output class: ", ncle,"/",ntot," (",round(ncle*100/ntot, 2),"%)","\n",sep="")
  }

  if(x$showid){

    if(noisetype(x$call[[1]]) == "att"){
      cat("\n## Indices of noisy samples per attribute:\n")
      for(a in 1:length(x$numnoise)){
        id <- paste0(x$idnoise[[a]], collapse = ", ")
        if(id == "")
          id <- "-"
        cat("- Attribute ", names(x$x)[a], ": ", id, "\n", sep = "")
      }
    }
    
    else if(noisetype(x$call[[1]]) == "com"){
      
      cat("\n## Indices of noisy samples per variable:\n")
      for(a in 1:(length(x$numnoise)-1)){
        id <- paste0(x$idnoise[[a]], collapse = ", ")
        if(id == "")
          id <- "-"
        cat("- Attribute ", names(x$x)[a], ": ", id, "\n", sep = "")
      }
      
      id <- paste0(x$idnoise[[length(x$numnoise)]], collapse = ", ")
      if(id == "")
        id <- "-"
      cat("- Output class: ", id, "\n", sep = "")
    }

    else{
      cat("\n## Indices of noisy samples:\n")
      id <- paste0(x$idnoise[[1]], collapse = ", ")
      if(id == "")
        id <- "-"
      cat("- Output class: ", id, "\n", sep = "")
    }

  }

}

###############################################################
###############################################################
###############################################################

#' Plot function for class ndmodel
#'
#' Representation of the dataset contained in an object of class \code{ndmodel} after the
#' application of a noise introduction model.
#'
#' This function performs a two-dimensional representation using the \code{ggplot2} package of
#' the dataset contained in the object \code{x} of class \code{ndmodel}.
#' Each of the classes in the dataset (available in \code{x$ynoise}) is represented by a
#' different color. There are two options to represent the input attributes of the samples
#' on the \emph{x} and \emph{y} axes of the graph:
#' \itemize{
#'   \item{If \code{pca = FALSE}, the values in the graph are taken from the current attribute
#'   values found in \code{x$xnoise}. In this case, \code{xvar} and \code{yvar} indicate the
#'   indices of the attributes to show in the \emph{x} and \emph{y} axes, respectively.}
#'   \item{If \code{pca = TRUE}, the values in the graph are taken after performing a PCA over
#'   \code{x$xnoise}. In this case, \code{xvar} and \code{yvar} indicate the index of the
#'   principal component according to the variance explained to show in the \emph{x} and \emph{y}
#'   axes, respectively.}
#' }
#' Finally, the parameter \code{noise} is used to indicate which samples (noisy, clean or all) to show.
#' Clean samples are represented by circles in the graph, while noisy samples are represented by crosses.
#'
#' @param x an object of class \code{ndmodel}.
#' @param ... other options to pass to the function.
#' @param noise a logical indicating which samples to show. The valid options are:
#' \itemize{
#'   \item{\code{TRUE}}{: to show only the noisy samples.}
#'   \item{\code{FALSE}}{: to show only the clean samples.}
#'   \item{\code{NA}}{: to show both the clean and noisy samples (default value).}
#' }
#' @param xvar an integer with the index of the input attribute (if \code{pca = FALSE}) or the
#' principal component (if \code{pca = TRUE}) to represent in the \emph{x} axis (default: 1).
#' @param yvar an integer with the index of the input attribute (if \code{pca = FALSE}) or the
#' principal component (if \code{pca = TRUE}) to represent in the \emph{y} axis (default: 2).
#' @param pca a logical indicating if PCA must be used (default: \code{FALSE}).
#'
#' @return An object of class \code{ggplot} and \code{gg} with the graph created using the
#' \code{ggplot2} package.
#'
#' @examples
#' # load the dataset
#' data(iris)
#' 
#' # apply the noise introduction model
#' set.seed(9)
#' output <- sym_uni_ln(x = iris[,-ncol(iris)], y = iris[,ncol(iris)], level = 0.1)
#' 
#' # plots for all the samples, the clean samples and the noisy samples using PCA
#' plot(output, pca = TRUE)
#' plot(output, noise = FALSE, pca = TRUE)
#' plot(output, noise = TRUE, pca = TRUE)
#' 
#' # plots using the Petal.Length and Petal.Width variables
#' plot(output, xvar = 3, yvar = 4)
#' plot(output, noise = FALSE, xvar = 3, yvar = 4)
#' plot(output, noise = TRUE, xvar = 3, yvar = 4)
#'
#' @seealso \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{sym_uni_ln}}, \code{\link{sym_cuni_ln}}, \code{\link{sym_uni_an}}
#' 
#' @export
#' @importFrom "stats" "prcomp" "predict"
#' @importFrom "grDevices" "rainbow" "colorRampPalette"
#' @importFrom "ggplot2" "ggplot" "aes" "geom_point" "xlim" "ylim" "xlab" "ylab" "labs"
#' "scale_color_manual" "theme" "element_rect" "element_text" "element_blank" "scale_shape_manual"
#' @importFrom "RColorBrewer" "brewer.pal"
plot.ndmodel <- function(x, ..., noise = NA, xvar = 1, yvar = 2, pca = FALSE){
  
  ######################################################
  # check for errors #########
  if(pca && any(sapply(X = x$xnoise, FUN = is.factor))){
    stop("\"plot\" is only available for numeric attributes")
  }
  if(!pca && (!is.numeric(x$xnoise[,xvar]) || !is.numeric(x$xnoise[,yvar])) ){
    stop("\"plot\" is only available for numeric attributes")
  }
  
  ######################################################
  if(noisetype(x$call[[1]]) == "att" || noisetype(x$call[[1]]) == "com"){
    idnoise <- unique(unlist(x$idnoise))
    idclean <- setdiff(1:nrow(x$xnoise), idnoise)
  }
  else{
    idnoise <- x$idnoise[[1]]
    idclean <- x$idclean[[1]]
  }

  if(pca){
    pca <- prcomp(~ ., data = x$xnoise, center = TRUE, scale = TRUE)
    var <- round((pca$sdev^2/sum(pca$sdev^2))*100,2)
    pred <- predict(pca, newdata = x$xnoise)
    data <- cbind(as.data.frame(pred), x$ynoise)
    xtag <- paste0("PC", xvar, " (", var[xvar], "%)")
    ytag <- paste0("PC", yvar, " (", var[yvar], "%)")
  }
  else{
    data <- cbind(x$xnoise, x$ynoise)
    xtag <- names(x$xnoise)[xvar]
    ytag <- names(x$xnoise)[yvar]
  }

  
  #colors_ <- rainbow(nlevels(x$ynoise))
  nc_ <- min(nlevels(x$ynoise), 8)
  if(nc_ < 3) nc_ <- 3
  colors_ <- brewer.pal(nc_, "Dark2")
  
  if(nlevels(x$ynoise) > 8)
    colors_ <- colorRampPalette(colors_)(nlevels(x$ynoise))
  
  sample.color <- colors_[as.integer(x$ynoise)]
  point.shape <- rep(1, nrow(x$xnoise))
  point.shape[idnoise] <- 2

  if(is.na(noise)){
    gr <- ggplot(data = data, aes(x = data[,xvar], y = data[,yvar], color = x$ynoise, shape = as.factor(point.shape))) +
      geom_point(stroke = 1.35) +
      xlim(min(data[,xvar]), max(data[,xvar])) +
      ylim(min(data[,yvar]), max(data[,yvar])) +
      xlab(xtag) +
      ylab(ytag) +
      labs(color='Class') +
      scale_color_manual(values = colors_) +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            aspect.ratio = 1,
            axis.text = element_text(colour = 1, size = 12),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"))

    if(length(idnoise) == nrow(x$xnoise)){
      gr <- gr + scale_shape_manual(name = "Sample", labels = c("Noisy"), values = c(4))
    }
    else if(length(idnoise) == 0){
      gr <- gr + scale_shape_manual(name = "Sample", labels = c("Clean"), values = c(16))
    }
    else{
      gr <- gr + scale_shape_manual(name = "Sample", labels = c("Clean", "Noisy"), values = c(16, 4))
    }
  }

  else{
    if(noise){
      gr <- ggplot(data = data[idnoise,], aes(x = data[idnoise,xvar], y = data[idnoise,yvar], color = x$ynoise[idnoise], shape = as.factor(point.shape[idnoise]))) +
        geom_point(stroke = 1.35) +
        xlim(min(data[,xvar]), max(data[,xvar])) +
        ylim(min(data[,yvar]), max(data[,yvar]))  +
        xlab(xtag) +
        ylab(ytag) +
        labs(color='Class') +
        scale_color_manual(values=colors_[sort(as.integer(unique(x$ynoise[idnoise])))]) +
        scale_shape_manual(name = "Sample", labels = c("Noisy"), values = c(4)) +
        theme(panel.border = element_rect(colour = "black", fill=NA),
              aspect.ratio = 1,
              axis.text = element_text(colour = 1, size = 12),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"))
    }

    if(!noise){
      gr <- ggplot(data = data[idclean,], aes(x = data[idclean,xvar], y = data[idclean,yvar], color = x$ynoise[idclean], shape = as.factor(point.shape[idclean]))) +
        geom_point(stroke = 1.35) +
        xlim(min(data[,xvar]), max(data[,xvar])) +
        ylim(min(data[,yvar]), max(data[,yvar]))  +
        xlab(xtag) +
        ylab(ytag) +
        labs(color='Class') +
        scale_color_manual(values=colors_[sort(as.integer(unique(x$ynoise[idclean])))]) +
        scale_shape_manual(name = "Sample", labels = c("Clean"), values = c(16)) +
        theme(panel.border = element_rect(colour = "black", fill=NA),
              aspect.ratio = 1,
              axis.text = element_text(colour = 1, size = 12),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"))
    }
  }

  return(gr)
}

###############################################################
###############################################################
###############################################################
