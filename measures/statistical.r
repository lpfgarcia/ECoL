# R Code
# Statistical Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of statistical measures from Statlog Project


perClass <- function(data) {

	aux = lapply(levels(data$class), 
		function(i) {
			data[data$class == i,]
	})

	return(aux)
}


covariance <- function(set, data) {

	tmp = combn(ncol(data)-1, 2)
	vet = colMeans(data[,-ncol(data)])

	aux = lapply(set, function(i) {
		apply(tmp, 2, function(j) {
			sum(((i[,j[1]] - vet[j[1]]) * 
			(i[,j[2]] - vet[j[2]])) / 
			nrow(data))
		})
	})

	return(aux)
}


correlation <- function(data) {

	aux = mapply(function(d) {
		tmp = cor(d[,-ncol(d)])
		mean(tmp[tmp != 1])
	}, d=data)

	return(mean(aux))
}


skewness <- function(data) {

	aux = mapply(function(d) {
		tmp = apply(d[,-ncol(d)], 2, base::skewness)
		mean(tmp)
	}, d=data)

	return(mean(aux))
}


kurtosis <- function(data) {

	aux = mapply(function(d) {
		tmp = apply(d[,-ncol(d)], 2, base::kurtosis)
		mean(tmp)
	}, d=data)

	return(mean(aux))
}


fraction <- function(data) {

	
}


statiscal <- function(data) {

	data = binarize(data)
	set = perClass(data)

	aux = lapply(STATISTICAL, 
		function(i) {
			do.call(i, list(set, data))
	})

	aux = unlist(aux)
	return(aux)
}