# R Code
# Config File 
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# Packages and Global Variables


require(ape)
require(cluster)
require(dplyr)
require(e1071)
require(foreign)


# directory
DIR = getwd()

# datasets
files = list.files(paste(DIR, "/database", sep=""), full.names = TRUE);

# linear measures
LINEARITY = c("l1", "l2", "l3")

# neighbor  measures
NEIGHBORHOOD = c("n1","n2")

# overlapping measures
OVERLAPPING = c("f1", "f2", "f3", "f4")

