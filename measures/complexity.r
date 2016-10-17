# R Code
# Complexity Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Complexity Measures


colMin <- function(data) {
	apply(data, 2, min)
}


colMax <- function(data) {
	apply(data, 2, max)
}


rowMax <- function(data) {
	apply(data, 1, max)
}


dist <- function(data) {
	as.matrix(daisy(data, metric = "gower", stand=TRUE))
}


form <- function(data) {

	att = paste(colnames(data)[-ncol(data)], collapse="+")
	aux = formula(paste("~ 0 +", att, sep=" "))
	return(aux)
}


binarize <- function(data) {

	aux = model.matrix(form(data), data)
	aux = data.frame(aux, class=data$class)
	return(aux)
}


ovo <- function(data) {

	aux = combn(levels(data$class), 2)

	tmp = apply(aux, 2, function(i) {
		vet = subset(data, data$class %in% i)
		vet$class = factor(vet$class)
		return(vet)
	})

	return(tmp)
}


complexity <- function(file) {

	data = read.arff(file)
	aux = c(fisher(data), linearity(data), 
		neighborhood(data), dimensionality(data))
	return(aux)
}

