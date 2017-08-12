# R Code
# Noise Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Noise Measures
#
#@inproceedings{van2007measures,
#  title={Measures for the characterisation of pattern-recognition data sets},
#  author={Van Der Walt, Christiaan and Barnard, Etienne},
#  year={2007},
#  organization={18th Annual Symposium of the Pattern Recognition Association of South Africa (PRASA)}
#}

# Input noise (N1) from Van der Walt, ID1)
# Count for each sample in how many dimensions it overlaps and then normalise the total overlap with the product of the number of samples in the data set and the dimensionality of the data set.
# First applies PCA to decorrelate the features
id1 <- function(data) {

    data = binarize(data)
    # ??? Why not scale. and center = TRUE?
    aux = data.frame(prcomp(data[,-ncol(data)])$x)
    aux$class = data$class
    aux = f2(aux)
    # ??? This is not counting the number of dimensions in which each sample overlap, but it is computing the volume of overlap instead
    # Definition: count for each sample in how many dimensions it overlaps and then normalise the total overlap with the product of the number of samples in the data set and the dimensionality of the data set.
    return(aux)
}

mutual <- function(data) {

    aux = sapply(data, function(i) {
        mutinformation(discretize(i), data$class)
    })

    aux = as.numeric(aux)
    return(aux)
}

dimen <- function(data) {
    aux = mutual(data)
    aux = sort(aux, decreasing = TRUE)[-1]
    aux = which(cumsum(aux)/sum(aux) >= 0.9)
    return(aux[1])
}

# Feature noise (ID2)
id2 <- function(data) {
    aux = (ncol(data) - 1 - dimen(data))/
    (ncol(data) - 1)
    return(aux) 
}


noise <- function(data) {

    aux = lapply(NOISE, 
        function(i) {
            do.call(i, list(data))
    })

    aux = unlist(aux)
    names(aux) = NOISE
    return(aux)
}

