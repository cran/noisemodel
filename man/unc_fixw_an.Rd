% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/069_unc_fixw_an.R
\name{unc_fixw_an}
\alias{unc_fixw_an}
\alias{unc_fixw_an.default}
\alias{unc_fixw_an.formula}
\title{Unconditional fixed-width attribute noise}
\usage{
\method{unc_fixw_an}{default}(x, y, level, k = 0.1, sortid = TRUE, ...)

\method{unc_fixw_an}{formula}(formula, data, ...)
}
\arguments{
\item{x}{a data frame of input attributes.}

\item{y}{a factor vector with the output class of each sample.}

\item{level}{a double in [0,1] with the noise level to be introduced in nominal attributes.}

\item{k}{a double in [0,1] with the domain proportion of the noise width (default: 0.1).}

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
Introduction of \emph{Unconditional fixed-width attribute noise} into a classification dataset.
}
\details{
\emph{Unconditional fixed-width attribute noise} corrupts all the samples in the dataset. 
For each attribute \emph{A}, all the original values are corrupted by adding a random number in the interval 
[-\emph{width}, \emph{width}], being \emph{width} = (\emph{max}(\emph{A})-\emph{min}(\emph{A}))??k. For 
nominal attributes, (\code{level}??100)\% of the samples in the dataset 
are chosen and a random value is selected as noisy.
}
\note{
Noise model adapted from the papers in References, corrupting all samples and 
allowing nominal attributes.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- unc_fixw_an(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- unc_fixw_an(formula = Species ~ ., data = iris2D, level = 0.1)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
A. Ramdas, B. Poczos, A. Singh, and L. A. Wasserman. 
\strong{An analysis of active learning with uniform feature noise}. 
In \emph{Proc. 17th International Conference on Artificial Intelligence and Statistics}, 
volume 33 of JMLR, pages 805-813, 2014.
url:\url{http://proceedings.mlr.press/v33/ramdas14.html}.
}
\seealso{
\code{\link{sym_end_an}}, \code{\link{sym_sgau_an}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
