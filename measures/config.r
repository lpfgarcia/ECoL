# R Code
# Config File 
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# Packages and Global Variables


require(ape)
require(cluster)
require(dplyr)
require(e1071)
require(foreign)
require(kknn)


# directory
DIR = getwd()

# datasets
files = list.files(paste(DIR, "/database", sep=""), full.names = TRUE)

# measures based on dimensionality
DIMENSIONALITY = c("t2")

# linear measures
LINEARITY = c("l1", "l2", "l3")

# neighbor  measures
NEIGHBORHOOD = c("n1","n2", "n3", "n4", "t1")

# overlapping measures
OVERLAPPING = c("f1", "f2", "f3", "f4")

