# R Code
# Measures based on dimensionality
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on dimensionality


m1 <- function(data) {
	log((ncol(data)-1)/nrow(data))
}


pca <- function(data) {

	data = binarize(data)
	aux = prcomp(data[,-ncol(data)])$sdev
	aux = which(cumsum(aux)/sum(aux) >= 0.95)
	return(aux[1])
}


m2 <- function(data) {
	log(pca(data)/nrow(data))
}


m3 <- function(data) {
	pca(data)/(ncol(data)-1)
}


dimensionality <- function(data) {

		aux = lapply(DIMENSIONALITY, 
		function(i) {
			do.call(i, list(data))
	})

	aux = unlist(aux)
	return(aux)
}

