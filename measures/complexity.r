# R Code
# Complexity Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Complexity Measures

# This file contains auxiliary functions used in the implementations

# Function for taking the minimum value in the columns of a data frame
colMin <- function(data) {
	apply(data, 2, min)
}

# Function for taking the maximum value in the columns of a data frame
colMax <- function(data) {
	apply(data, 2, max)
}

# Function for taking the maxmum value in the rows of a data frame
rowMax <- function(data) {
	apply(data, 1, max)
}

# Function for computing the distance matrix between all pairs of examples in a data frame (uses the gower heterogeneous distance metric)
dist <- function(data) {
	as.matrix(daisy(data, metric = "gower", stand = TRUE))
}

# ???
form <- function(data) {

	att = paste(colnames(data)[-ncol(data)], collapse="+")
	aux = formula(paste("~ 0 +", att, sep=" "))
	return(aux)
}

# ???
binarize <- function(data) {

	aux = model.matrix(form(data), data)
	aux = data.frame(aux, class=data$class)
	return(aux)
}

# Function for One-Versus-One decomposition of multiclass problems (decomposes a multiclass problem into all pairwise sub-problems)
ovo <- function(data) {

	aux = combn(levels(data$class), 2) # all pairwise combinations of the classes

	tmp = apply(aux, 2, function(i) {
		vet = subset(data, data$class %in% i)
		vet$class = factor(vet$class)
		return(vet)
	})

	return(tmp)
}

# Function for applying all complexity measures types into a data frame
complexity <- function(data) {
	aux = c(fisher(data), linearity(data), 
		neighborhood(data), dimensionality(data))
	return(aux)
}

