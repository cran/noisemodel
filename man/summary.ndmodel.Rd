% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndmodel.R
\name{summary.ndmodel}
\alias{summary.ndmodel}
\title{Summary function for class ndmodel}
\usage{
\method{summary}{ndmodel}(object, ..., showid = FALSE)
}
\arguments{
\item{object}{an object of class \code{ndmodel}.}

\item{...}{other options to pass to the function.}

\item{showid}{a logical indicating if the indices of noisy samples must be displayed (default: \code{FALSE}).}
}
\value{
A list with the elements of \code{object}, including the \code{showid} argument.
}
\description{
This method displays a summary containing information about the noise
introduction process contained in an object of class \code{ndmodel}.
}
\details{
This function presents a summary containing information of the noise introduction process and the resulting
noisy dataset contained in the object \code{object} of class \code{ndmodel}.
The information offered is as follows:
\itemize{
   \item the function call.
   \item the name of the noise introduction model.
   \item the parameters associated with the noise model.
   \item the number of noisy and clean samples in the dataset.
   \item the number of noisy samples per class/attribute.
   \item the number of clean samples per class/attribute.
   \item the indices of the noisy samples (if \code{showid = TRUE}).
}
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- sym_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)

}
\seealso{
\code{\link{print.ndmodel}}, \code{\link{plot.ndmodel}}, \code{\link{sym_uni_ln}}, \code{\link{sym_cuni_ln}}, \code{\link{sym_uni_an}}
}
