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

### Run the complexity measures for the datasets

```
source("~/DCoL/run.r")
run()

```
### Run the experiments for a given dataset

```
source("~/DCoL/run.r")

data = iris
data$class = data$Species
data$Species = NULL

complexity(data)
```
