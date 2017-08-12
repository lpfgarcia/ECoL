# R Code
# Measures based on Density
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on density
#
# These measures are based on the paper: 
#@article{sotoca2006meta,
#  title={A meta-learning framework for pattern classication by means of data complexity measures},
#  author={Sotoca, Jos{\'e} Mart{\'\i}nez and Mollineda, Ram{\'o}n Alberto and S{\'a}nchez, Jos{\'e} Salvador},
#  journal={Inteligencia Artificial. Revista Iberoamericana de Inteligencia Artificial},
#  volume={10},
#  number={29},
#  year={2006},
#  publisher={Asociaci{\'o}n Espa{\~n}ola para la Inteligencia Artificial}
#}


# Auxiliary function for computing D2
volume <- function(data) {
    data = data[,-ncol(data)] # removing the label column
    prod(colMax(data) - colMin(data)) # taking the volume
}

# Volume of local neighborhood (D2)
# This measure represents the average volume occupied by the k nearest neighbors of each training instance
d2 <- function(dst, data, k=3) {

    aux = unlist(
        lapply(rownames(data), 
            function(i) {
                tmp = knn(dst, data, k, i) # taking the 3 nearest neighbors of each example 
                volume(data[names(tmp),]) # computing the volume they occupy
        })
    )
         
    return(mean(aux)) # takes the average of the volumes
}


voting <- function(pred, data) {

    if(max(table(pred)) >= 2) {
        return(which.max(table(pred)))
    } else {
        return(data[i,]$class)
    }
}

# Auxiliary function for computing D3
lying <- function(pred, data) {
    aux = table(pred, data$class) # comparing the majority voting predictions to the actual class of each instance
    sum(aux) - sum(diag(aux)) # taking the number of erroneous predictions
}

# Class density in overlap region (D3)
# The aim of this measure is to determine the den- sity of each class in the overlap regions. Based on Wilson’s editing.
# Finds the k nearest neighbors of each example (xi,ωi). If a majority of these k neighbors belong to a class different from ωi, we can consider that (xi,ωi) lies in an overlap region. 
# D3 can be measured by counting, for each class, the number of points lying in the region of some different class.
d3 <- function(dst, data, k=3) {

    aux = unlist(
        lapply(rownames(data), 
            function(i) {
                tmp = knn(dst, data, k, i) # taking the k nearest neighbors of each example
                voting(tmp, data) # taking the majority of the neighbors labels
        })
    )

    aux = lying(names(aux), data)
    return(aux)
}


density <- function(data) {

    data = binarize(data)
    dst = dist(data[,-ncol(data)])

    aux = lapply(DENSITY, 
        function(i) {
            do.call(i, list(dst, data))
    })

    aux = unlist(aux)
    names(aux) = DENSITY
    return(aux)
}

