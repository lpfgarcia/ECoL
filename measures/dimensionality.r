# R Code
# Measures based on dimensionality
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on dimensionality


t2 <- function(data) {
	aux = nrow(data)/(ncol(data)-1)
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

