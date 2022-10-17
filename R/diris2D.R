###############################################################
###############################################################
###############################################################

#' diris2D dataset
#'
#' Discretized version of the \code{iris2D} dataset.
#'
#' @docType data
#'
#' @usage data(diris2D)
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
#' 
#' # load the dataset
#' data(diris2D)
#' 
#' # noise introduction
#' set.seed(9)
#' outdef <- sym_uni_ln(x = diris2D[,-ncol(diris2D)], y = diris2D[,ncol(diris2D)], level = 0.1)
#' 
#' # show results
#' summary(outdef, showid = TRUE)
#' 
#' @seealso \code{\link{iris2D}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
#' 
"diris2D"

###############################################################
###############################################################
###############################################################
