% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/029_qua_uni_ln.R
\name{qua_uni_ln}
\alias{qua_uni_ln}
\alias{qua_uni_ln.default}
\alias{qua_uni_ln.formula}
\title{Quadrant-based uniform label noise}
\usage{
\method{qua_uni_ln}{default}(x, y, level, att1 = 1, att2 = 2, sortid = TRUE, ...)

\method{qua_uni_ln}{formula}(formula, data, ...)
}
\arguments{
\item{x}{a data frame of input attributes.}

\item{y}{a factor vector with the output class of each sample.}

\item{level}{a double vector with the noise levels in [0,1] in each quadrant.}

\item{att1}{an integer with the index of the first attribute forming the quadrants (default: 1).}

\item{att2}{an integer with the index of the second attribute forming the quadrants (default: 2).}

\item{sortid}{a logical indicating if the indices must be sorted at the output (default: \code{TRUE}).}

\item{...}{other options to pass to the function.}

\item{formula}{a formula with the output class and, at least, one input attribute.}

\item{data}{a data frame in which to interpret the variables in the formula.}
}
\value{
An object of class \code{ndmodel} with elements:
\item{xnoise}{a data frame with the noisy input attributes.}
\item{ynoise}{a factor vector with the noisy output class.}
\item{numnoise}{an integer vector with the amount of noisy samples per class.}
\item{idnoise}{an integer vector list with the indices of noisy samples.}
\item{numclean}{an integer vector with the amount of clean samples per class.}
\item{idclean}{an integer vector list with the indices of clean samples.}
\item{distr}{an integer vector with the samples per class in the original data.}
\item{model}{the full name of the noise introduction model used.}
\item{param}{a list of the argument values.}
\item{call}{the function call.}
}
\description{
Introduction of \emph{Quadrant-based uniform label noise} into a classification dataset.
}
\details{
For each sample, the probability of flipping its label is based on which quadrant 
(with respect to the attributes \code{att1} and \code{att2}) the sample falls in. 
The probability of mislabeling for each quadrant is expressed with the argument \code{level}, 
whose length is equal to 4. 
Let \emph{m1} and \emph{m2} be the mean values of the domain of \code{att1} and \code{att2}, respectively. 
Each quadrant is defined as follows: values <= \emph{m1} 
and <= \emph{m2} (first quadrant); values <= \emph{m1} and > \emph{m2} (second quadrant); 
values > \emph{m1} and <= \emph{m2} (third quadrant); and values > \emph{m1} 
and > \emph{m2} (fourth quadrant). Finally, the labels of these samples are randomly 
replaced by other different ones within the set of class labels.
}
\note{
Noise model adapted from the papers in References.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- qua_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], 
                       level = c(0.05, 0.15, 0.20, 0.4))

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- qua_uni_ln(formula = Species ~ ., data = iris2D, 
                        level = c(0.05, 0.15, 0.20, 0.4))

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
A. Ghosh, N. Manwani, and P. S. Sastry. 
\strong{Making risk minimization tolerant to label noise}. 
\emph{Neurocomputing}, 160:93-107, 2015.
\doi{10.1016/j.neucom.2014.09.081}.
}
\seealso{
\code{\link{exps_cuni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
