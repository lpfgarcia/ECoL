# DCoL

Data Complexity Library in R

## Technical Requirements

R version 3.4.1 (2017-06-30) -- "Single Candle"

Packages: cluster, dplyr, e1071, foreign, igraph, infotheo and MASS

## Set the experiments

Install the packages:

```
install.packages(c("cluster", "dplyr", "e1071", "foreign", "igraph", "infotheo", "MASS"))

```

## Run the complexity measures for the datasets

```
source("~/DCoL/run.r")
run()

```
## Run the experiments for a given dataset

```
source("~/DCoL/run.r")

data = iris
data$class = data$Species
data$Species = NULL

complexity(data)
```

### Contact

Luis Paulo Faina Garcia - lpfgarcia [at] gmail.com

University of São Paulo - Campus São Carlos


### References

[1] Ho, T. and Basu, M. (2002). Complexity measures of supervised classification problems. IEEE Trans. on Pattern Analysis and Machine Intelligence, 24(3):289-300.

[2] Vanschoren, J., Rijn, J., Bischl, B., and Torgo, Luis. (2013) OpenML: networked science in machine learning. SIGKDD Explorations, 15(2):49-60.

[4]   R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.  URL https://www.R-project.org/.

