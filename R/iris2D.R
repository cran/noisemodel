###############################################################
###############################################################
###############################################################

#' iris2D dataset
#'
#' A 2-dimensional version of the well-known \code{\link[datasets]{iris}} dataset. It maintains the 
#' attributes \code{Petal.Length} and \code{Petal.Width}, which give the measurements in centimeters of 
#' the petal length and width of iris flowers belonging to three different species (\emph{setosa}, \emph{versicolor} and 
#' \emph{virginica}). Duplicate and contradictory samples are removed from the dataset, resulting in a total 
#' of 103 samples.
#'
#' @docType data
#'
#' @usage data(iris2D)
#'
#' @format A data.frame with 103 samples (rows) and 3 variables (columns) named Petal.Length, Petal.Width and Species.
#'
#' @keywords datasets
#'
#' @references 
#' R. A. Fisher. \strong{The use of multiple measurements in taxonomic problems}. 
#' \emph{Annals of Eugenics}, 7:179-188, 1936.
#' 
#' E. Anderson. \strong{The irises of the Gaspe Peninsula}. 
#' \emph{Bulletin of the American Iris Society}, 59:2-5, 1935.
#'
#' @source Data collected by E. Anderson (1935).
#'
#' @examples
#' library(ggplot2)
#' library(RColorBrewer)
#' 
#' data(iris2D)
#' 
#' ggplot(data = iris2D, aes(x = iris2D[,1], y = iris2D[,2], color = iris2D[,3])) +
#'    geom_point(stroke = 0.5) +
#'    xlim(min(iris2D[,1]), max(iris2D[,1])) +
#'    ylim(min(iris2D[,2]), max(iris2D[,2])) +
#'    xlab(names(iris2D)[1]) + 
#'    ylab(names(iris2D)[2]) +
#'    labs(color='Species') +
#'    scale_color_manual(values = brewer.pal(3, "Dark2")) +
#'    theme(panel.border = element_rect(colour = "black", fill=NA),
#'          aspect.ratio = 1,
#'          axis.text = element_text(colour = 1, size = 12),
#'          legend.background = element_blank(),
#'          legend.box.background = element_rect(colour = "black"))
#' 
#' @seealso \code{\link{sym_uni_ln}}, \code{\link{sym_uni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#' 
"iris2D"

###############################################################
###############################################################
###############################################################
