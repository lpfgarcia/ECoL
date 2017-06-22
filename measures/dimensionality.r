# R Code
# Measures based on dimensionality
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on dimensionality

# These measures are described in:
#@article{lorena2012analysis,
#  title={Analysis of complexity indices for classification problems: Cancer gene expression data},
#  author={Lorena, Ana C and Costa, Ivan G and Spola{\^o}r, Newton and De Souto, Marcilio CP},
#  journal={Neurocomputing},
#  volume={75},
#  number={1},
#  pages={33--42},
#  year={2012},
#  publisher={Elsevier}
#}

# Log of the average number of features per example (M1 or T2')
# This measure is similar to T2
# Caution: this measure will take negative values for datasets containing more examples than features 
m1 <- function(data) {
	log((ncol(data)-1)/nrow(data))
}

# Auxiliary function for taking the number of components representing 95% of data variability in PCA
pca <- function(data) {
# ????
	data = binarize(data) # why do you need this?
	# usually I use center = TRUE and scale. = TRUE in PCA
	aux = prcomp(data[,-ncol(data)])$sdev
	aux = which(cumsum(aux)/sum(aux) >= 0.95)
	return(aux[1]) # taking the number of components representing 95% of data variability
}

# Principal component dimensionality/sample ratio (M2)
# Log of the ratio of the number of components representing 95% of data variability and the number of examples in a dataset
# Caution: this measure will take negative values for datasets containing more examples than PCA components explaining 95% of data variability
# ??? Is it better to remove the log?
m2 <- function(data) {
	log(pca(data)/nrow(data))
}

# Principal component dimensionality/dimensionality ratio (M3)
# Ratio of the number of components representing 95% of data variability and the number of original features in a dataset
m3 <- function(data) {
	pca(data)/(ncol(data)-1)
}

# Auxiliary function for applying the measures
dimensionality <- function(data) {

	aux = lapply(DIMENSIONALITY, 
		function(i) {
			do.call(i, list(data))
	})

	aux = unlist(aux)
	names(aux) = DIMENSIONALITY
	return(aux)
}

