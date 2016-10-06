# R Code
# Measures based on Neighborhood
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on Neighborhood


n1 <- function(dst, data) {

	tree = mst(dst)
	i = which(tree != 0, arr.ind = TRUE)
	aux = sum(data$class[i[,1]] != data$class[i[,2]])/2
	return(aux/nrow(data))
}


n2 <- function(dst, data) {




}

neighborhood <- function(data) {

	dst = dist(data[,-ncol(data)])

		aux = lapply(NEIGHBORHOOD, 
		function(i) {
			do.call(i, list(dst, data))
	})

	aux = unlist(aux)
	return(aux)
}

