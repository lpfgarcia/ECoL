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


pca <- function(data) {

	aux = prcomp(data[,-ncol(data)])$sdev
	aux = which(cumsum(aux)/sum(aux) > 0.95)
}


m3 <- function(data) {
	data = binarize(data)
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

