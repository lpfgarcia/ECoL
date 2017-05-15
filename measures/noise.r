# R Code
# Noise Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Noise Measures


id1 <- function(data) {

	data = binarize(data)
	aux = data.frame(prcomp(data[,-ncol(data)])$x)
	aux$class = data$class
	aux = f2(aux)
	return(aux)
}


mutual <- function(data) {

	aux = sapply(data, function(i) {
		mutinformation(discretize(i), data$class)
	})

	aux = as.numeric(aux)
	return(aux)
}


dimen <- function(data) {
	aux = mutual(data)
	aux = sort(aux, decreasing = TRUE)[-1]
	aux = which(cumsum(aux)/sum(aux) >= 0.9)
	return(aux[1])
}


id2 <- function(data) {
	aux = (ncol(data) - 1 - dimen(data))/
	(ncol(data) - 1)
	return(aux) 
}


noise <- function(data) {

	aux = lapply(NOISE, 
		function(i) {
			do.call(i, list(data))
	})

	aux = unlist(aux)
	names(aux) = NOISE
	return(aux)
}

