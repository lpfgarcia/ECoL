# R Code
# Config File 
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# Packages and Global Variables


require(cluster)
require(dplyr)
require(e1071)
require(foreign)
require(igraph)
require(infotheo)


# directory
DIR = getwd()

# datasets
files = list.files(paste(DIR, "/database", sep=""), full.names = TRUE)

# measures based on dimensionality
DIMENSIONALITY = c("t2")

# density measures
DENSITY = c("d1", "d2", "d3")

# linear measures
LINEARITY = c("l1", "l2", "l3")

# neighborhood measures
NEIGHBORHOOD = c("n1","n2", "n3", "n4", "t1", "t2", "t4")

# noise measures
NOISE = c("id1", "id2")

# overlapping measures
OVERLAPPING = c("f1", "f2", "f3", "f4")

