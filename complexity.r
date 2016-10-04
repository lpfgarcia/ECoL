# R Code
# Complexity Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Complexity Measures


require(dplyr)
require(e1071)


colMin <- function(data) {
	apply(data, 2, min)
}


colMax <- function(data) {
	apply(data, 2, max)
}


rowMax <- function(data) {
	apply(data, 1, max)
}


form <- function(data) {

	att = paste(colnames(data)[1:(ncol(data)-1)], collapse="+")
	aux = formula(paste("~ 0 +", att, sep=" "))
	return(aux)
}


preprocessing <- function(data) {

	aux = model.matrix(form(data), data)
	aux = data.frame(aux, class=data$class)
	return(aux)
}


ovo <- function(data) {

	aux = combn(levels(data$class), 2)

	tmp = lapply(1:ncol(aux), function(i) {
			vet = subset(data, data$class %in% aux[,i])
			vet$class = factor(vet$class, labels=1:2)
			return(vet)
		}
	)

	return(tmp)
}


complexity <- function(data) {

	aux = c(fisher(data), linearity(data))
	return(aux)
}

