# R Code
# Measures based on dimensionality
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on dimensionality
#
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


pca <- function(data) {

    aux = prcomp(data[,-ncol(data)])$sdev
    aux = which(cumsum(aux)/sum(aux) >= 0.95)
    return(aux[1])
}


m3 <- function(data) {
    pca(data)/(ncol(data)-1)
}


dimensionality <- function(data) {

    data = binarize(normalize(data))
    aux = lapply(DIMENSIONALITY, 
        function(i) {
            do.call(i, list(data))
    })

    aux = unlist(aux)
    names(aux) = DIMENSIONALITY
    return(aux)
}

