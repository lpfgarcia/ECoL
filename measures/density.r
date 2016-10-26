# R Code
# Measures based on Density
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on density


volume <- function(data) {
	data = data[,-ncol(data)]
	prod(colMax(data) - colMin(data))
}


d2 <- function(dst, data, k=3) {

	aux = unlist(
		lapply(rownames(data), 
			function(i) {
				tmp = knn(dst, data, k, i)
				volume(data[names(tmp),])
		})
	)

	return(mean(aux))
}


voting <- function(pred, data) {

	if(max(table(pred)) >= 2) {
		return(which.max(table(pred)))
	} else {
		return(data[i,]$class)
	}
}


lying <- function(pred, data) {
	aux = table(pred, data$class)
	sum(aux) - sum(diag(aux))
}


d3 <- function(dst, data, k=3) {

	aux = unlist(
		lapply(rownames(data), 
			function(i) {
				tmp = knn(dst, data, k, i)
				voting(tmp, data)
		})
	)

	aux = lying(names(aux), data)
	return(aux)
}


density <- function(data) {

	dst = dist(data[,-ncol(data)])

	aux = lapply(DENSITY, 
		function(i) {
			do.call(i, list(dst, data))
	})

	aux = unlist(aux)
	return(aux)
}

