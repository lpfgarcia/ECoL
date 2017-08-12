# R Code
# Complexity Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Complexity Measures


colMin <- function(data) {
    apply(data, 2, min)
}


colMax <- function(data) {
    apply(data, 2, max)
}


rowMax <- function(data) {
    apply(data, 1, max)
}


dist <- function(data) {
    as.matrix(daisy(as.matrix(data), metric="gower", stand=TRUE))
}


form <- function(data) {

    att = paste(colnames(data)[-ncol(data)], collapse="+")
    aux = formula(paste("~ 0 +", att, sep=" "))
    return(aux)
}


binarize <- function(data) {

    aux = model.matrix(form(data), data)
    aux = data.frame(aux, class=data$class)
    return(aux)
}


normalize <- function(data) {

    for(i in 1:ncol(data)) {
        if(is.numeric(data[,i])) {
            data[,i] = scale(data[,i])
        }
    }

    return(data)
}


ovo <- function(data) {

    aux = combn(levels(data$class), 2)

    tmp = apply(aux, 2, function(i) {
        vet = subset(data, data$class %in% i)
        vet$class = factor(vet$class)
        return(vet)
    })

    return(tmp)
}


complexity <- function(data) {

    aux = c(fisher(data), linearity(data), 
        neighborhood(data), dimensionality(data))
    return(aux)
}

