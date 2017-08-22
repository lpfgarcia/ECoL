# R Code
# Config File 
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# Packages and Global Variables

# Required packages
require(cluster)
require(dplyr)
require(e1071)
require(foreign)
require(igraph)
require(infotheo)
require(MASS)


# directory
DIR = getwd()

# datasets
files = list.files(paste(DIR, "/database", sep=""), full.names=TRUE)

# dimensionality measures 
DIMENSIONALITY = c("m1", "m2", "m3", "c1")

# density measures
GEODENSITY = c("d2", "d3")

# linear measures
LINEARITY = c("l1", "l2", "l3")

# local set measures
LOCALSET = c("LSCAvg")

# neighborhood measures
NEIGHBORHOOD = c("n1","n2", "n3", "n4", "t1", "t2", "t4")

# graph measures
NETWORK = c("edges", "avg_degree", "density", "max_componet", 
    "avg_closeness", "avg_betweenness", "avg_hub", 
    "cluster_coefficient", "avg_path_length")

# overlapping measures
OVERLAPPING = c("f1", "f2", "f3", "f4")

