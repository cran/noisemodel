% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/071_sym_cuni_cn.R
\name{sym_cuni_cn}
\alias{sym_cuni_cn}
\alias{sym_cuni_cn.default}
\alias{sym_cuni_cn.formula}
\title{Symmetric completely-uniform combined noise}
\usage{
\method{sym_cuni_cn}{default}(x, y, level, sortid = TRUE, ...)

\method{sym_cuni_cn}{formula}(formula, data, ...)
}
\arguments{
\item{x}{a data frame of input attributes.}

\item{y}{a factor vector with the output class of each sample.}

\item{level}{a double in [0,1] with the noise level to be introduced.}

\item{sortid}{a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).}

\item{...}{other options to pass to the function.}

\item{formula}{a formula with the output class and, at least, one input attribute.}

\item{data}{a data frame in which to interpret the variables in the formula.}
}
\value{
An object of class \code{ndmodel} with elements:
\item{xnoise}{a data frame with the noisy input attributes.}
\item{ynoise}{a factor vector with the noisy output class.}
\item{numnoise}{an integer vector with the amount of noisy samples per variable.}
\item{idnoise}{an integer vector list with the indices of noisy samples per variable.}
\item{numclean}{an integer vector with the amount of clean samples per variable.}
\item{idclean}{an integer vector list with the indices of clean samples per variable.}
\item{distr}{an integer vector with the samples per class in the original data.}
\item{model}{the full name of the noise introduction model used.}
\item{param}{a list of the argument values.}
\item{call}{the function call.}
}
\description{
Introduction of \emph{Symmetric completely-uniform combined noise} into a classification dataset.
}
\details{
\emph{Symmetric completely-uniform combined noise} corrupts (\code{level}·100)\% of the values of 
each attribute in the dataset. In order to corrupt an attribute \emph{A}, (\code{level}·100)\% of the
samples in the dataset are randomly chosen. Then, their values for \emph{A} are replaced by random ones 
from the domain of the attribute.

Additionally, this noise model also selects (\code{level}·100)\% of the samples
in the dataset with independence of their class. The labels of these samples are randomly
replaced by other ones within the set of class labels.

Note that, for both attributes and 
class labels, the original value of a sample can be chosen as noisy and the actual percentage 
of noise in the dataset can be lower than the theoretical noise level.
}
\note{
Noise model adapted from the papers in References.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- sym_cuni_cn(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- sym_cuni_cn(formula = Species ~ ., data = iris2D, level = 0.1)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
C. Teng. \strong{Polishing blemishes: Issues in data correction}. 
\emph{IEEE Intelligent Systems}, 19(2):34-39, 2004. 
\doi{10.1109/MIS.2004.1274909}.
}
\seealso{
\code{\link{uncs_guni_cn}}, \code{\link{sym_cuni_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
