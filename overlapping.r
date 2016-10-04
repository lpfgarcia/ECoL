# R Code
# Overlapping Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Overlapping Measures


num <- function(data, j) {

	aux = nrow(data[data$class == j,]) * 
		((colMeans(data[data$class == j, -ncol(data)]) - 
			colMeans(data[,-ncol(data)]))^2)
	return(aux)
}


den <- function(data, j) {

	aux = rowSums((t(data[data$class == j,-ncol(data)]) - 
		colMeans(data[data$class == j,-ncol(data)]))^2)
	return(aux)
}


f1 <- function(data) {

	tmp = lapply(
		levels(data$class), function(j){
			num(data, j)/den(data, j)
	})

	aux = do.call("rbind", tmp)
	aux = colSums(aux)
	return(max(aux))
}


overlap <- function(data) {

	l = levels(data$class)
	a = data[data$class == l[1], -ncol(data), drop = FALSE]
	b = data[data$class == l[2], -ncol(data), drop = FALSE]

	aux = colMin(rbind(colMax(a), colMax(b))) - 
		colMax(rbind(colMin(a), colMin(b)))

	aux = colMax(rbind(aux, 0))
	return(aux)
}


range <- function(data) {

	l = levels(data$class)
	a = data[data$class == l[1], -ncol(data), drop = FALSE]
	b = data[data$class == l[2], -ncol(data), drop = FALSE]

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


nonOverlap <- function(data) {

	l = levels(data$class)
	a = data[data$class == l[1], -ncol(data), drop = FALSE]
	b = data[data$class == l[2], -ncol(data), drop = FALSE]

	maxmin = colMax(rbind(colMin(a), colMin(b)))
	minmax = colMin(rbind(colMax(a), colMax(b)))

	aux = do.call("cbind",
		lapply(1:(ncol(data)-1), function(i) {
			data[,i] < maxmin[i] | data[,i] > minmax[i]
		})
	)

	aux = data.frame(aux)
	rownames(aux) = rownames(data)
	return(aux)
}


f3 <- function(data) {

	data = ovo(data);

	aux = do.call("rbind",
		lapply(data, function(tmp) {
			colSums(nonOverlap(tmp))/nrow(tmp)
		})
	)

	aux = mean(rowMax(aux))
	return(aux)
}


removing <- function(data) {

	repeat {
		tmp = nonOverlap(data)
		col = which.max(colSums(tmp))
		aux = rownames(tmp[tmp[,col] != TRUE, , drop = FALSE])
		data = data[aux,- col, drop = FALSE]
		if(nrow(data) == 0 | ncol(data) == 1)
			break
	}

	return(data)
}


f4 <- function(data) {

	data = ovo(data);

	aux = do.call("rbind",
		lapply(data, function(tmp) {
			new = removing(tmp)
			nrow(tmp) - nrow(new)
		})
	)

	aux = mean(aux)
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

