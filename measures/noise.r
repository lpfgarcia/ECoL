# R Code
# Noise Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Noise Measures


dimensionality <- function(data) {

	aux = sapply(data, function(i) {
		mutinformation(discretize(i), data$class)
	})

	aux = as.numeric(aux)
	return(aux)
}


id <- function(data) {
	aux = dimensionality(data)
	aux = sort(aux, decreasing = TRUE)[-1]
	aux = which(cumsum(aux)/sum(aux) > 0.9)
	return(aux)
}


id2 <- function(data) {
	aux = (ncol(data) - 1 - id(data))/(ncol(data) - 1)
	return(aux) 
}


noise <- function(data) {

	aux = lapply(NOISE, 
		function(i) {
			do.call(i, list(data))
	})

	aux = unlist(aux)
	return(aux)
}
