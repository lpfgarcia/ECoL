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


volume <- function(data) {
    data = data[,-ncol(data)]
    prod(colMax(data) - colMin(data))
}


# Volume of local neighborhood (D2)
d2 <- function(dst, data, k=3) {

    aux = unlist(
        lapply(rownames(data),
            function(i) {
                tmp = knn(dst, data, k, i)
                volume(data[names(tmp),])
        })
    )

    return(mean(aux))
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
    aux = table(pred, data$class)
    sum(aux) - sum(diag(aux))
}


# Class density in overlap region (D3)
d3 <- function(dst, data, k=3) {

    aux = unlist(
        lapply(rownames(data),
            function(i) {
                tmp = knn(dst, data, k, i)
                voting(tmp, data)
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

