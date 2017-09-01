# ECoL

The Extended Complexity Library (ECoL) is the implementation in R of a set of measures to characterize the complexity of classification problems based on ambiguity and separation between the classes and the data sparsity and dimensionality of the datasets. The measures were originally proposed by Ho and Basu (2002) and extend by many other works including the [DCoL](https://github.com/nmacia/dcol). The main difference between the libraries is that ECoL provides bug fixes, generalizations and implementations of many other state-of-the-art measures.

## Measures



**Measures of overlapping** 

* F1: Fisher's discriminant ratio
* F1v: The directional-vector Fisher's discriminant ratio
* F2: Overlapping of the per-class bounding boxes
* F3: Maximum individual feature efficiency
* F4: Cllective feature efficiency

**Measures of neighborhood information** 

* LSCAvg: Local-Set cardinality average
* N1: Fraction of points lying on the class boundary
* N2: Average intra/inter class nearest neighbor distances
* N3: Leave-one-out error rate of the 1-nearest neighbor algorithm
* N4: Nonlinearity of the one-nearest neighbor classifier
* T1: Fraction of maximum covering spheres on data
* T2: 
* T4:

**Measures of linearity** 

* L1: Distance of erroneous instances to a linear classifier
* L2: Training error of a linear classifier
* L3: Nonlinearity of a linear classifier

**Measures of dimensionality**

* M1:
* M2:
* M3:
* C1:

**Measures of density**

* D1:
* D2:
* D3:

**Measures of structural representation**

* Edges: Number of edges
* Avg_degree: Average degree of the network
* Avg_density: Average density of network
* Max_componet: Maximum number of components
* Avg_closeness: Average closeness centrality
* Avg_betweenness: Average betweenness centrality
* Avg_hub: Average hub score
* Cluster_coefficient: Clustering Coefficient
* Avg_path_length: Average Path Length

## Technical Requirements

The requirements to run the ECoL are R version >= 3.4.1 and the packages `dplyr`, `e1071` and `igraph`. The [R Project](https://www.r-project.org/) provide a full installation guide for Linux, Windows and Mac operating systems.

## Installation

It is possible to install the development version using:

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("lpfgarcia/ECoL")
library("ECoL")
```

## Example of use

After download the code of ECoL, we need to load the complexity measures:

```r
library(ECoL)
data("iris")

## Extract all complexity measures using formula
iris.info <- metafeatures(Species ~ ., iris, groups="all")

```

## Contact

To submit bugs and feature requests, report at [project issues](https://github.com/lpfgarcia/ECoL/issues).

## References

[1] Ho, T. and Basu, M. (2002). Complexity measures of supervised classification problems. IEEE Trans. on Pattern Analysis and Machine Intelligence, 24(3):289-300.

[2] Orriols-Puig, A., Maciá, N., Ho, T. (2010). Documentation for the data complexity library in C++. Technical report, La Salle - Universitat Ramon Llull.

[3]   R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.  URL https://www.R-project.org/.

