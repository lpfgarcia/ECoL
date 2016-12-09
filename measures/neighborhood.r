# R Code
# Measures based on Non Linearity
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on Non Linearity


n1 <- function(dst, data) {

	g = graph.adjacency(dst, weighted = TRUE)
	tree = as.matrix(as_adj(mst(as.undirected(g))))

	tmp = which(tree != 0, arr.ind = TRUE)
	aux = sum(data[tmp[,1],]$class != data[tmp[,2],]$class)/2
	return(aux/nrow(data))
}


intra <- function(dst, data, i) {

	tmp = rownames(data[data$class == data[i,]$class,])
	aux = sort(dst[i, setdiff(tmp, i)])[1]
	return(aux)
}


inter <- function(dst, data, i) {

	tmp = rownames(data[data$class != data[i,]$class,])
	aux = sort(dst[i, tmp])[1]
	return(aux)
}


n2 <- function(dst, data) {

	aux = sapply(rownames(data), 
		function(i) {
			a = intra(dst, data, i)
			r = inter(dst, data, i)
			return(a/r)
	})

	aux[aux == Inf] = NA
	aux = sum(aux, na.rm=TRUE)
	return(aux)
}


knn <- function(dst, data, k, i) {
	tmp = names(sort(dst[i,])[1:k+1])
	aux = data[tmp,]$class
	names(aux) = tmp
	return(aux)
}

n3 <- function(dst, data) {

	aux = unlist(
		lapply(rownames(data), 
			function(i) {
				knn(dst, data, 1, i) != data[i,]$class
		})
	)

	return(mean(aux))
}


n4 <- function(dst, data) {

	aux = rbind(data, generate(data))
	vet = setdiff(rownames(aux), rownames(data))
	dst = dist(aux[,-ncol(aux)])

	aux = unlist(
		lapply(vet, function(i) {
			idx = which.min(dst[i, rownames(data)])
			data[names(idx),]$class != aux[i,]$class
		})
	)

	return(mean(aux))
}


radios <- function(dst, data, i) {

	di = inter(dst, data, i)
	j = names(di)
	dj = inter(dst, data, j)

	if(di == dj) { 
		return(di/2)
	} else {
		tmp = radios(dst, data, j)
		return(dj - tmp)
	}
}


hyperspher <- function(dst, data) {

	r = rep(0, nrow(data))
	names(r) = rownames(data) 

	for(i in names(r)){
		r[i] = radios(dst, data, i)
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

	h = n = c()

	repeat{

		aux = which.max(rowSums(adh))
		tmp = names(which(adh[aux,]))
		dif = setdiff(rownames(adh), tmp)
		adh = adh[dif, dif]

		h = c(h, length(tmp))
		n = c(n, names(aux))

		if(is.null(dim(adh)) | sum(adh) == 0 |
			all(dim(adh) == 0))
				break
	}

	names(h) = n
	return(h)
}


t1 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)
	aux = length(aux)/nrow(data)
	return(aux)
}


t2 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)
	return(mean(aux))
}


ball <- function(r, n) {
	(1/sqrt(n*pi))*((2*pi*exp(1)/n)^(n/2))*r^n
}


t4 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)
	tmp = aux/ball(r[names(aux)], ncol(data)-1)
	return(mean(tmp))
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

