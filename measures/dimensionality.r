# R Code
# Measures based on dimensionality
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on dimensionality


m1 <- function(data) {
	(ncol(data)-1)/nrow(data)
}


m2 <- function(data) {
	log((ncol(data)-1)/nrow(data))
}


m3 <- function(data) {
	data = binarize(data)
	aux = data.frame(prcomp(data[,-ncol(data)])$x)
	aux$class = data$class
	aux = m1(aux)
	return(aux)
}



dimensionality <- function(data) {

		aux = lapply(DIMENSIONALITY, 
		function(i) {
			do.call(i, list(data))
	})

	aux = unlist(aux)
	return(aux)
}

