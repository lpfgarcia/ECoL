# ECoL

The Extended data Complexity Library (ECoL) is the implementation in R of a set of measures to characterize the complexity of classification problems based on ambiguity and separation between the classes and the data sparsity and dimensionality of the datasets. The measures were originally proposed by Ho and Basu (2002) and extend by many other works including the [DCoL](https://github.com/nmacia/dcol). The main difference between the libraries is that ECoL provides bug fixes, generalizations and implementations of many other state-of-the-art measures.

### Technical Requirements

The requirements to run the ECoL are:
  1. R version >= 3.4.1
  2. Packages: `dplyr`, `e1071` and `igraph`. 

The [R Project](https://www.r-project.org/) provide a full installation guide for Linux, Windows and Mac operating systems. To install the dependence packages run the code:

```
install.packages(c("dplyr", "e1071", "igraph"))
```

### Load the complexity measures:

After download the code of ECoL, we need to load the complexity measures:

```
source("~/DCoL/run.r")
```

### Run the complexity measures for a given dataset

```
data = iris
data$class = data$Species
data$Species = NULL

complexity(data)
```

### Contact

Luis P. F. Garcia: garcia [at] informatik [dot] uni-leipzig [dot] de

Ana C. Lorena: aclorena [at] unifesp [dot] br

Marcilio de Souto: marcilio [dot] desouto [at] univ-orleans [dot] fr


### References

[1] Ho, T. and Basu, M. (2002). Complexity measures of supervised classification problems. IEEE Trans. on Pattern Analysis and Machine Intelligence, 24(3):289-300.

[2] Vanschoren, J., Rijn, J., Bischl, B. and Torgo, Luis. (2013). OpenML: networked science in machine learning. SIGKDD Explorations, 15(2):49-60.

[3]   R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.  URL https://www.R-project.org/.

