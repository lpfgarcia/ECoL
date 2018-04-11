# ECoL
[![Travis-CI Build Status](https://travis-ci.org/SmartDataAnalytics/ECoL.svg?branch=master)](https://travis-ci.org/rSmartDataAnalytics/ECoL)

The Extended Complexity Library (ECoL) is the implementation in R of a set of measures to characterize the complexity of classification problems based on the ambiguity and the separation between the classes and the data sparsity and dimensionality of the datasets. The measures were originally proposed by Ho and Basu (2002) and extend by many other works including the [DCoL](https://github.com/nmacia/dcol) library. The main difference between the libraries is that ECoL provides bug fixes, generalizations and implementations of many other state-of-the-art measures.

## Measures

The measures can be divided into the following groups: (1) feature overlapping measures, (2) neighborhood measures, (3) linearity measures, (4) dimensionality measures, (5) class balance measures and (6) network measures. 

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
* LSCAvg: Local-Set cardinality average

**Measures of linearity** 

* L1: Distance of erroneous instances to a linear classifier
* L2: Training error of a linear classifier
* L3: Nonlinearity of a linear classifier

**Measures of dimensionality**

* T2: Average number of samples per dimension
* T3: Average intrinsic dimensionality  per number of examples
* T4: Intrinsic dimensionality proportion

**Measures of class balance**

* C1: Class balance
* C2: Multi-class imbalance ratio

**Measures of structural representation**

* Density: Average density of network
* ClsCoef: Clustering Coefficient
* Hubs: Average hub score

## Installation

The installation process is similar to other packages available on CRAN:

```r
install.packages("ECoL")
```

It is possible to install the development version using:

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("SmartDataAnalytics/ECoL")
library("ECoL")
```

## Example of use

The simplest way to compute the complexity measures are using the `complexity` method. The method can be called by a symbolic description of the model or by a data frame. The parameters are the dataset and the group of measures to be extracted. The default paramenter is extract all the measures. To extract a specific measure, use the function related with the group. A simple example is given next:

```r
## Extract all complexity measures using formula
complexity(Species ~ ., iris)

## Extract all complexity measures using data frame
complexity(iris[,1:4], iris[,5])

## Extract the overlapping measures
complexity(Species ~ ., iris, groups="overlapping")

## Extract the F1 measure using overlapping function
overlapping(Species ~ ., iris, measures="F1")
```

## Developer notes

To cite `ECoL` in publications use: Garcia, L. P. F., Lorena, A. C., and Lehmann J. (2018). ECoL: Extended complexity library. R package version 0.1.0, https://CRAN.Rproject.org/package=ECoL.

To submit bugs and feature requests, report at [project issues](https://github.com/SmartDataAnalytics/ECoL/issues).

## References

[1] Ho, T., and Basu, M. (2002). Complexity measures of supervised classification problems. IEEE Transactions on Pattern Analysis and Machine Intelligence, 24(3):289-300.

[2] Orriols-Puig, A., Maciá, N., and Ho, T. (2010). Documentation for the data complexity library in C++. Technical report, La Salle - Universitat Ramon Llull.

[3] R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
