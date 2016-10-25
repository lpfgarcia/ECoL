# R Code
# Measures based on density
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on density


d1 <- function(data) {



}


volume <- function(data) {
	data = data[,-ncol(data)]
	prod(colMax(data) - colMin(data))
}


d2 <- function(dst, data, k=3) {

	aux = lapply(rownames(data), 
		function(i) {
			nk = names(sort(dst[i,])[2:(k+1)])
			volume(data[nk,])
	})

	aux = mean(unlist(aux))
	return(aux)
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

	pred = unlist(
		lapply(rownames(data), 
			function(i) {
				pred = knn(dst, data, k, i)
				voting(pred, data)
		})
	)

	aux = lying(names(pred), data)
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

