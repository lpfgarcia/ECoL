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


intra <- function(dst, data, i) {

	tmp = rownames(data[data$class == data[i,]$class,])
	aux = min(dst[i, setdiff(tmp, i)])
	return(aux)
}


inter <- function(dst, data, i) {

	tmp = rownames(data[data$class != data[i,]$class,])
	aux = min(dst[i, setdiff(tmp, i)])
	return(aux)
}


n2 <- function(dst, data) {

	aux = unlist(
		lapply(rownames(data), 
			function(i) {
				intra(dst, data, i)/inter(dst, data, i)
		})
	)

	return(sum(aux))
}


knn <- function(dst, data, i) {
	tmp = setdiff(rownames(dst), i)
	aux = data[names(which.min(dst[i, tmp])),]$class
	return(aux)
}


n3 <- function(dst, data) {

	aux = unlist(
		lapply(rownames(data), 
			function(i) {
				knn(dst, data, i) != data[i,]$class
		})
	)

	return(sum(aux))
}


hyperspher <- function(dst, data) {

	aux = lapply(rownames(data), 
			function(i) {
				dst[i,] < 0.55 *inter(dst, data, i)
		})

}


t1 <- function(dst, data) {

	aux = hyperspher(dst, data)/nrow(data)
	return(aux)
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

