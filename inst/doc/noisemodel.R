## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

getInfo <- function(what = "Suggests") {
  text <- packageDescription("noisemodel")[what][[1]]
  text <- gsub("\n", ", ", text, fixed = TRUE)
  text <- gsub(">=", "$\\\\ge$", text, fixed = TRUE)
  eachPkg <- strsplit(text, ", ", fixed = TRUE)[[1]]
  eachPkg <- gsub(",", "", eachPkg, fixed = TRUE)
  #out <- paste("\\\**", eachPkg[order(tolower(eachPkg))], "}", sep = "")
  #paste(out, collapse = ", ")
  length(eachPkg)
}

## ----install1-----------------------------------------------------------------
# install.packages("noisemodel")

## ----install2-----------------------------------------------------------------
library(noisemodel)

## ----help---------------------------------------------------------------------
# help(sym_uni_ln)

## ----example 1----------------------------------------------------------------
# load the dataset
data(iris2D)

# usage of the default method
set.seed(9)
outdef <- sym_uni_ln(x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], level = 0.1)

# show results
summary(outdef, showid = TRUE)
plot(outdef)

# usage of the method for class formula
set.seed(9)
outfrm <- sym_uni_ln(formula = Species ~ ., data = iris2D, level = 0.1)

# check the match of noisy indices
identical(outdef$idnoise, outfrm$idnoise)

## ----example 2----------------------------------------------------------------
str(outdef)

## ----example 3----------------------------------------------------------------
print(outdef)

## ----example 4----------------------------------------------------------------
summary(outdef, showid = TRUE)

## ----example 5----------------------------------------------------------------
plot(outdef)

