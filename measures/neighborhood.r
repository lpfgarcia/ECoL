# R Code
# Measures based on Neighborhood
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on Neighborhood


n1 <- function(dst, data) {

	g = graph.adjacency(dst, weighted = TRUE)
	tree = as.matrix(as_adj(mst(as.undirected(g))))

	tmp = which(tree != 0, arr.ind = TRUE)
	aux = sum(data[tmp[,1],]$class != data[tmp[,2],]$class)/2
	return(aux/nrow(data))
}


intra <- function(dst, data, i) {

	tmp = rownames(data[data$class == data[i,]$class,])
	aux = min(dst[i, setdiff(tmp, i)])
	return(aux)
}


inter <- function(dst, data, i) {

	tmp = rownames(data[data$class != data[i,]$class,])
	aux = min(dst[i, tmp])
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
	tmp = setdiff(rownames(data), i)
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


n4 <- function(dst, data) {

	tmp = do.call("rbind",
		lapply(1:nrow(data), function(i) {
			interpolation(data)
		})
	)

	aux = rbind(data, tmp)
	dst = dist(aux[,-ncol(aux)])
	vet = setdiff(rownames(aux), rownames(data))

	aux = unlist(
		lapply(vet, 
			function(i) {
				idx = which.min(dst[i, rownames(data)])
				data[names(idx),]$class != aux[i,]$class
		})
	)

	return(mean(aux))
}


radios <- function(dst, data, r, i) {

	di = inter(dst, data, i)
	j = names(which(dst[i,] == di)[1])
	dj = inter(dst, data, j)

	if(di == dj) { 
		r[i] = di/2
		return(r[i])
	} else {
		tmp = radios(dst, data, r, j)
		r[i] = di - tmp
	}

	return(r[i])
}


hyperspher <- function(dst, data) {

	r = rep(0, nrow(data))
	names(r) = rownames(data) 

	for(i in names(r)){
		r[i] = radios(dst, data, r, i)
	}

	return(r)
}


translate <- function(dst, r) {

	aux = do.call("rbind",
		lapply(rownames(dst), 
			function(i) {
				dst[i,] < r[i]
		})
	)

	rownames(aux) = rownames(dst)
	return(aux)
}


adherence <- function(adh, data) {

	h = 0

	repeat{

		aux = sort(rowSums(adh), decreasing=TRUE)
		tmp = names(which(adh[names(aux[1]),] == TRUE))
		dif = setdiff(rownames(adh), tmp)
		adh = adh[dif, dif]
		h = h + 1
		if(is.null(dim(adh)) | 
			all(dim(adh) == 0))
			break
	}

	return(h)
}


t1 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)/nrow(data)
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

