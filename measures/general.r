# R Code
# General Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of general measures from Statlog Project


col <- function(data) {
	ncol(data) -1 
}


nclass <- function(data) {
	nlevels(data$class)
}

