# R Code
# Space Partitioning Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Space Partitioning Measures



cutdata <- function(x, b) {
    cut(x, b, include.lowest=TRUE)
}


subsample <- function(data, tmp, rule) {

    aux = lapply(rownames(data), function(i) {
        if(all(tmp[i,-ncol(data)] == rule))
            return(i)
    })

    aux = data[unlist(aux),]
    if(nrow(aux) == 0)
        return(NULL)
    aux$class = factor(aux$class)
    return(aux)
}


partitioning <- function(data) {

    tmp = apply(data[,-ncol(data)], 2, function(i) {
        aux = c(min(i), median(i), max(i))
        cutdata(i, aux)
    })

    tmp = data.frame(tmp, class=data$class)

    aux = lapply(1:4, function(i) {
        levels(tmp[,i])
    })

    aux = expand.grid(aux)

    subset = apply(aux, 1, function(i) {
        subsample(data, tmp, i)
    })

    aux = Filter(Negate(is.null), subset)
    return(aux)
}


ce <- function(data, subset) {

    aux = lapply(subset, function(l) {
        i = table(l$class)/nrow(l)
        sum(-i*log(i))*nrow(l)/nrow(data)
    })

    aux = 1 - sum(unlist(aux))
    return(aux)
}



space <- function(data) {

    subset = partitioning(data)

    aux = lapply(SPACE, 
        function(i) {
            do.call(i, list(data, subset))
    })

    aux = unlist(aux)
    names(aux) = SPACE
    return(aux)
}