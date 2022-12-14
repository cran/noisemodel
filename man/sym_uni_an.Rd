% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/064_sym_uni_an.R
\name{sym_uni_an}
\alias{sym_uni_an}
\alias{sym_uni_an.default}
\alias{sym_uni_an.formula}
\title{Symmetric uniform attribute noise}
\usage{
\method{sym_uni_an}{default}(x, y, level, sortid = TRUE, ...)

\method{sym_uni_an}{formula}(formula, data, ...)
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
\item{numnoise}{an integer vector with the amount of noisy samples per attribute.}
\item{idnoise}{an integer vector list with the indices of noisy samples per attribute.}
\item{numclean}{an integer vector with the amount of clean samples per attribute.}
\item{idclean}{an integer vector list with the indices of clean samples per attribute.}
\item{distr}{an integer vector with the samples per class in the original data.}
\item{model}{the full name of the noise introduction model used.}
\item{param}{a list of the argument values.}
\item{call}{the function call.}
}
\description{
Introduction of \emph{Symmetric uniform attribute noise} into a classification dataset.
}
\details{
\emph{Symmetric uniform attribute noise} corrupts (\code{level}·100)\% of the values of 
each attribute in the dataset. In order to corrupt an attribute \emph{A}, (\code{level}·100)\% of the
samples in the dataset are randomly chosen. Then, their values for \emph{A} are replaced by random 
different ones from the domain of the attribute.
}
\note{
Noise model adapted from the papers in References.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- sym_uni_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- sym_uni_an(formula = Species ~ ., data = iris2D, level = 0.1)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
J. A. Sáez, M. Galar, J. Luengo, and F. Herrera.
\strong{Tackling the problem of classification with noisy data using Multiple Classifier Systems: Analysis of the performance and robustness}. 
\emph{Information Sciences}, 247:1-20, 2013.
\doi{10.1016/j.ins.2013.06.002}.
}
\seealso{
\code{\link{sym_cuni_an}}, \code{\link{sym_cuni_cn}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
