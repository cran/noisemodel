% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/035_sym_cen_ln.R
\name{sym_cen_ln}
\alias{sym_cen_ln}
\alias{sym_cen_ln.default}
\alias{sym_cen_ln.formula}
\title{Symmetric center-based label noise}
\usage{
\method{sym_cen_ln}{default}(x, y, level, sortid = TRUE, ...)

\method{sym_cen_ln}{formula}(formula, data, ...)
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
Introduction of \emph{Symmetric center-based label noise} into a classification dataset.
}
\details{
\emph{Symmetric center-based label noise} randomly selects (\code{level}·100)\% of the samples
in the dataset with independence of their class. The probability for chosing the noisy label 
is determined based on the distance between class centers.
Thus, the mislabeling probability between classes increases as the distance between their 
centers decreases. This model is consistent with the intuition that samples in similar 
classes are more likely to be mislabeled. Besides, the model also allows mislabeling 
data in dissimilar classes with a relatively small probability, which corresponds to 
label noise caused by random errors.
}
\note{
Noise model adapted from the papers in References.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- sym_cen_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- sym_cen_ln(formula = Species ~ ., data = iris2D, level = 0.1)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
X. Pu and C. Li. 
\strong{Probabilistic information-theoretic discriminant analysis for industrial 
label-noise fault diagnosis}. 
\emph{IEEE Transactions on Industrial Informatics}, 17(4):2664-2674, 2021.
\doi{10.1109/TII.2020.3001335}.
}
\seealso{
\code{\link{glev_uni_ln}}, \code{\link{sym_hienc_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
