% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/030_sco_con_ln.R
\name{sco_con_ln}
\alias{sco_con_ln}
\alias{sco_con_ln.default}
\alias{sco_con_ln.formula}
\title{Score-based confidence label noise}
\usage{
\method{sco_con_ln}{default}(x, y, level, sortid = TRUE, ...)

\method{sco_con_ln}{formula}(formula, data, ...)
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
Introduction of \emph{Score-based confidence label noise} into a classification dataset.
}
\details{
\emph{Score-based confidence label noise} follows the intuition that hard samples are 
more likely to be mislabeled. Given the confidence per class of each sample, 
if it is predicted with a different class with a high probability, it means that 
it is hard to clearly distinguish the sample from this class. The confidence information is used to compute a mislabeling score for each sample and its potential noisy 
label. Finally, (\code{level}??100)\% of the samples with the highest mislabeling scores 
are chosen as noisy.
}
\note{
Noise model adapted from the papers in References.
}
\examples{
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- sco_con_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- sco_con_ln(formula = Species ~ ., data = iris2D, level = 0.1)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

}
\references{
P. Chen, J. Ye, G. Chen, J. Zhao, and P. Heng. 
\strong{Beyond class-conditional assumption: A primary attempt to combat instance-dependent label noise}. 
In \emph{Proc. 35th AAAI Conference on Artificial Intelligence}, pages 11442-11450, 2021.
url:\url{https://ojs.aaai.org/index.php/AAAI/article/view/17363}.
}
\seealso{
\code{\link{mis_pre_ln}}, \code{\link{smam_bor_ln}}, \code{\link{print.ndmodel}}, \code{\link{summary.ndmodel}}, \code{\link{plot.ndmodel}}
}
