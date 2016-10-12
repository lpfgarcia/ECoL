# R Code
# Measures based on Neighborhood
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on Neighborhood


n1 <- function(dst, data) {

	tree = mst(dst)
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

	dst = dist(tmp[,-ncol(tmp)])

	aux = unlist(
		lapply(rownames(tmp), 
			function(i) {
				knn(dst, tmp, i) != tmp[i,]$class
		})
	)

	return(sum(aux))
}


eps <- function(dst, data) {

	delta = unlist(
		lapply(rownames(dst), function(i) 
			inter(dst, data, i)
		)
	)

	aux = 0.55 * min(delta)
	return(aux)
}


radios <- function(dst, data, r, i) {
	tmp = data[i,]$class != data$class
	aux = all(dst[i, tmp] > r[i] + r[tmp])
	return(aux)
}


hyperspher <- function(dst, data) {

	e = eps(dst, data)
	r = rep(e, nrow(data))
	names(r) = rownames(data) 

	repeat {
		h = 0
		for(i in 1:nrow(dst)) {
			if(radios(dst, data, r, i))
				r[i] = r[i] + e
			else
				h = h + 1
		}
		if(h == nrow(data))
			break
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

