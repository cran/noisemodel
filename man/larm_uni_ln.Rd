% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/016_larm_uni_ln.R
\name{larm_uni_ln}
\alias{larm_uni_ln}
\alias{larm_uni_ln.default}
\alias{larm_uni_ln.formula}
\title{Large-margin uniform label noise}
\usage{
\method{larm_uni_ln}{default}(x, y, level, sortid = TRUE, ...)

\method{larm_uni_ln}{formula}(formula, data, ...)
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
Introduction of \emph{Large-margin uniform label noise} into a classification dataset.
}
\details{
\emph{Large-margin uniform label noise} uses an SVM to induce the decision border 
in the dataset. For each sample, its distance
to the decision border is computed. Then, the samples are ordered according to their distance and 
(\code{level}·100)\% of the most distant correctly classified samples to the decision boundary 
are selected to be mislabeled with a random different class.
}
\note{
Noise model adapted from the papers in References to multiclass data, considering SVM with linear 
kernel as classifier.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- larm_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.3)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- larm_uni_ln(formula = Species ~ ., data = iris2D, level = 0.3)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
E. Amid, M. K. Warmuth, and S. Srinivasan. 
\strong{Two-temperature logistic regression based on the Tsallis divergence}.
In \emph{Proc. 22nd International Conference on Artificial Intelligence and Statistics}, 
volume 89 of PMLR, pages 2388-2396, 2019. 
url:\url{http://proceedings.mlr.press/v89/amid19a.html}.
}
\seealso{
\code{\link{hubp_uni_ln}}, \code{\link{smu_cuni_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
