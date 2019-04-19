# ECoL
[![Travis-CI Build Status](https://travis-ci.org/lpfgarcia/ECoL.svg?branch=master)](https://travis-ci.org/lpfgarcia/ECoL)
[![codecov](https://codecov.io/gh/lpfgarcia/ECoL/branch/master/graph/badge.svg)](https://codecov.io/gh/lpfgarcia/ECoL)
[![CRAN](https://www.r-pkg.org/badges/version/ECoL)](https://CRAN.R-project.org/package=ECoL)

The Extended Complexity Library (ECoL) is the implementation in R of a set of measures to characterize the complexity of classification and regression problems based on aspects that quantify the linearity of the data, the presence of informative feature, the sparsity and dimensionality of the datasets. The measures were originally proposed by Ho and Basu [1] and extend by many other works including the DCoL library [2]. The main difference between the libraries is that ECoL provides bug fixes, generalizations and implementations of many other state-of-the-art measures.

## Measures

The measures can be divided into two groups: classification and regression measures. The classification measures are based on:  (1) feature overlapping measures, (2) neighborhood measures, (3) linearity measures, (4) dimensionality measures, (5) class balance measures and (6) network measures. The regression measures are based on: (3) linearity measures, (4) dimensionality measures, (7) correlation measures and (8) smoothness measures.

**Measures of overlapping** 

* F1: Fisher's discriminant ratio
* F1v: The directional-vector Fisher's discriminant ratio
* F2: Overlapping of the per-class bounding boxes
* F3: Maximum individual feature efficiency
* F4: Cllective feature efficiency

**Measures of neighborhood information** 

* N1: Fraction of points lying on the class boundary
* N2: Average intra/inter class nearest neighbor distances
* N3: Leave-one-out error rate of the 1-nearest neighbor algorithm
* N4: Nonlinearity of the one-nearest neighbor classifier
* T1: Fraction of maximum covering spheres on data
* LSC: Local-Set cardinality average

**Measures of linearity** 

* L1: Distance of erroneous instances to a linear classifier
* L2: Training error of a linear classifier
* L3: Nonlinearity of a linear classifier

**Measures of dimensionality**

* T2: Average number of samples per dimension
* T3: Average intrinsic dimensionality  per number of examples
* T4: Intrinsic dimensionality proportion

**Measures of class balance**

* C1: Entropy of class proportions
* C2: Multi-class imbalance ratio

**Measures of structural representation**

* Density: Average density of network
* ClsCoef: Clustering Coefficient
* Hubs: Average hub score

**Measures of feature correlation**

* C2: Feature correlation to the output
* C3: Individual feature efficiency
* C4: Collective feature efficiency

**Measures of smoothness**

* S1: Output distribution
* S2: Input distribution
* S3: Error of a nearest neighbor regressor
* S4: Non-linearity of nearest neighbor regressor

## Installation

The installation process is similar to other packages available on CRAN:

```r
install.packages("ECoL")
```

It is possible to install the development version using:

```r
if(!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("lpfgarcia/ECoL")
library("ECoL")
```

## Example of use

The simplest way to compute the complexity measures are using the `complexity` method. The method can be called by a symbolic description of the model or by a data frame. The parameters are the dataset the group of measures to be extracted and the summarization functions. If it is a classification task, the response needs to be a factor, otherwise the package will assume a regression task. The default paramenter is extract all the measures. To extract a specific measure, use the function related with the group. A simple example is given next:

```r
## Extract all complexity measures for classification task
complexity(Species ~ ., iris)

## Extract all complexity measures for regression task
complexity(speed ~., cars)

## Extract all complexity measures using data frame for classification task
complexity(iris[,1:4], iris[,5])

## Extract the overlapping measures
complexity(Species ~ ., iris, groups="overlapping")

## Extract the F1 measure using overlapping function
overlapping(Species ~ ., iris, measures="F1")
```
Several measures return more than one value. To aggregate the returned values, post processed methods can be used. This method can compute `min`, `max`, `mean`, `median`, `kurtosis`, `standard deviation`, among others (see the `post.processing` documentation for more details). The default methods are the `mean` and the `sd`. Next, it is possible to see an example of the use of this method:

```r
## Extract all measures using min, median and max for classification task
complexity(Species ~ ., iris, summary=c("min", "median", "max"))

## Extract all measures using min, median and max for regression task
complexity(speed ~., cars, summary=c("min", "median", "max"))

```

## Developer notes

To cite `ECoL` in publications use: 

* Lorena, A. C., Garcia, L. P. F., Lehmann, J., de Souto, M. C. P., and Ho, T. K. (2018). How Complex is your classification problem? A survey on measuring classification complexity. arXiv:1808.03591

* Lorena, A. C., Maciel, A. I., de Miranda, P. B. C., Costa, I. G., and Prudêncio, R. B. C. (2018). Data complexity meta-features for regression problems. Machine Learning, 107(1):209-246.

To submit bugs and feature requests, report at [project issues](https://github.com/lpfgarcia/ECoL/issues).

## References

[1] Ho, T., and Basu, M. (2002). Complexity measures of supervised classification problems. IEEE Transactions on Pattern Analysis and Machine Intelligence, 24(3):289-300.

[2] Orriols-Puig, A., Maciá, N., and Ho, T. (2010). Documentation for the data complexity library in C++. Technical report, La Salle - Universitat Ramon Llull.

[3] R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
