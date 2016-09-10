# R Code
# Overlapping Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Overlapping Measures


numf1 <- function(data, j) {

	aux = nrow(data[data$class == j,]) * 
		((colMeans(data[data$class == j, -ncol(data)]) - 
			colMeans(data[,-ncol(data)]))^2)
	return(aux)
}


denf1 <- function(data, j) {

	aux = rowSums((t(data[data$class == j,-ncol(data)]) - 
		colMeans(data[data$class == j,-ncol(data)]))^2)
	return(aux)
}


f1 <- function(data) {

	tmp = lapply(levels(data$class), function(j){
		numf1(data, j)/denf1(data, j)
	})

	aux = do.call("rbind", tmp)
	aux = colSums(aux)
	return(max(aux))
}


overlap <- function(data) {

	l = levels(data$class)
	a = data[data$class == l[1], -ncol(data)]
	b = data[data$class == l[2], -ncol(data)]

	aux = colMin(rbind(colMax(a), colMax(b))) - 
		colMax(rbind(colMin(a), colMin(b)))

	aux = colMax(rbind(aux, 0))
	return(aux)
}


range <- function(data) {

	l = levels(data$class)
	a = data[data$class == l[1], -ncol(data)]
	b = data[data$class == l[2], -ncol(data)]

	aux = colMax(rbind(colMax(a), colMax(b))) - 
		colMin(rbind(colMin(a), colMin(b)))

	return(aux)
}


f2 <- function(data) {

	data = ovo(data);

	aux = unlist(
		lapply(data, function(tmp) {
			tmp = overlap(tmp)/range(tmp)
			prod(tmp, na.rm=TRUE)
		})
	)

	return(sum(aux))
}


noverlap <- function(data) {

	l = levels(data$class)
	a = data[data$class == l[1], -ncol(data)]
	b = data[data$class == l[2], -ncol(data)]

	aux = rowSums( t(data[,-ncol(data)]) < 
		colMax(rbind(colMin(a), colMin(b))) | 
		t(data[,-ncol(data)]) > 
		colMin(rbind(colMax(a), colMax(b))))
	return(aux)
}


f3 <- function(data) {

	data = ovo(data);

	aux = do.call("rbind",
		lapply(data, function(tmp) {
			noverlap(tmp)/nrow(tmp)
		})
	)

	aux = mean(rowMax(aux))
	return(aux)
}


fisher <- function(data) {

	data = preprocessing(data)

	aux = lapply(
		c("f1", "f2", "f3", "f4"), function(i) {
			do.call(i, list(data))
		}
	)

	aux = unlist(aux)
	return(aux)
}

