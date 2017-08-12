# R Code
# Measures based on Non Linearity
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on Non Linearity

# Implements measures from:
#@article{ho2002complexity,
#  title={Complexity measures of supervised classification problems},
#  author={Ho, Tin Kam and Basu, Mitra},
#  journal={IEEE transactions on pattern analysis and machine intelligence},
#  volume={24},
#  number={3},
#  pages={289--300},
#  year={2002},
#  publisher={IEEE}
#}

# and 

# @inproceedings{van2007measures,
#  title={Measures for the characterisation of pattern-recognition data sets},
#  author={Van Der Walt, Christiaan and Barnard, Etienne},
#  year={2007},
#  organization={18th Annual Symposium of the Pattern Recognition Association of South Africa (PRASA)}
#}

# Fraction of points on class boundary (N1)
n1 <- function(dst, data) {

	g = graph.adjacency(dst, weighted = TRUE) # adjacency graph between all pairs of examples in a data set
	tree = as.matrix(as_adj(mst(as.undirected(g)))) # taking the minimum spanning tree of the previous graph

	tmp = which(tree != 0, arr.ind = TRUE)	
	aux = which(data[tmp[,1],]$class != data[tmp[,2],]$class) # taking the connected examples in the MST that have different classes
	aux = length(unique(tmp[aux,1]))
	return(aux/nrow(data))
}

# Distance of an example to its nearest neighbor from the same class
intra <- function(dst, data, i) {

	tmp = rownames(data[data$class == data[i,]$class,])
	aux = sort(dst[i, setdiff(tmp, i)])[1]
	return(aux)
}

# Distance of an example to its nearest neighbor from a different class
inter <- function(dst, data, i) {

	tmp = rownames(data[data$class != data[i,]$class,])
	aux = sort(dst[i, tmp])[1]
	return(aux)
}

# Ratio of average intra/inter class NN distance (N2)
n2 <- function(dst, data) {

	aux = sapply(rownames(data), 
		function(i) {
			a = intra(dst, data, i)
			r = inter(dst, data, i)
			return(c(a,r))
	})

	aux = sum(aux[1,])/sum(aux[2,])
	return(aux)
}

# Nearest neighor classifier
# ??? why did you not use an implementation from the R packages??
knn <- function(dst, data, k, i) {
	tmp = names(sort(dst[i,])[1:k+1]) # you do not have to test setdiff(tmp, i)????
	aux = data[tmp,]$class
	names(aux) = tmp
	return(aux) 
}

# Error rate of 1NN classifier (N3)
n3 <- function(dst, data) {

	aux = unlist(
		lapply(rownames(data), 
			function(i) {
				knn(dst, data, 1, i) != data[i,]$class
		})
	)

	return(mean(aux))
}

# Nonlinearity of 1NN classifier
# ??? Will you generate the same data as for L3? Probably not, so wouldn't it be interesting to set the same seed for both??
n4 <- function(dst, data) {

	aux = rbind(data, generate(data)) # generating interpolated data
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

# Auxiliary function for computing T1
# Computes the radios of the hyperspheres that can be formed around each example
radios <- function(dst, data, i) {

	di = inter(dst, data, i) # finding i's nearest example from the other class
	j = names(di) # index of this example
	dj = inter(dst, data, j) # finding i's nearest example from the other class
	k = names(dj) # index of the previous example

	if(i == k) { # if they are mutually the nearest neighbors from different classes
		return(di/2) # their radios is half the distance between them
	} else {
		tmp = radios(dst, data, j) # the radios of j must be found recursively
		return(dj - tmp) # after the radios of j is found, the radios of i can be updated
	}
}

# Auxiliary function for computing T1
# Calls the radios function for all examples in a data frame
hyperspher <- function(dst, data) {

	r = rep(0, nrow(data)) # initialization
	names(r) = rownames(data) 

	for(i in names(r)){
		r[i] = radios(dst, data, i)
	}

	return(r)
}

# Auxiliary function for computing T1
# ??? verifying which hyperspheres can be eliminated
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

# Auxiliary function for computing T1
# ???
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

# Fraction of points with associated adherence subsets retained (T1)
t1 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)
	aux = length(aux)/nrow(data)
	return(aux)
}

# Samples per group (T2 from Van der Walts)
# average number of samples per group (adherence subset)
t2 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)
	return(mean(aux))
}

# Auxiliary function for computing T4
# The radius of a sphere is the Euclidean distance from the sphere centre to the furthest sample in the sphere.
# ??? I do not know if it is according to the previous definition
ball <- function(r, n) {
	(1/sqrt(n*pi))*((2*pi*exp(1)/n)^(n/2))*r^n
}

# Inter-class scale variation (part of T4 from Van der Walts) 
# density of the hyper-spheres retained
# SD of sphere densities is T4, here it is pho from Equation 3
t4 <- function(dst, data) {

	r = hyperspher(dst, data)
	aux = adherence(translate(dst, r), data)
	tmp = aux/ball(r[names(aux)], ncol(data)-1)
	return(sd(tmp))
}

# Applying all neighborhood functions
neighborhood <- function(data) {

	dst = dist(data[,-ncol(data)])

	aux = lapply(NEIGHBORHOOD, 
		function(i) {
			do.call(i, list(dst, data))
	})

	aux = unlist(aux)
	names(aux) = NEIGHBORHOOD
	return(aux)
}

