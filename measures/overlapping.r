# R Code
# Overlapping Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Overlapping Measures
#
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
#
#@article{sotoca2005review,
#  title={A review of data complexity measures and their applicability to pattern classification problems},
#  author={Sotoca, JM and Sanchez, JS and Mollineda, RA},
#  journal={Actas del III Taller Nacional de Mineria de Datos y Aprendizaje. TAMIDA},
#  pages={77--83},
#  year={2005}
#}
#
#@article{orriols2010documentation,
#  title={Documentation for the data complexity library in C++},
#  author={Orriols-Puig, Albert and Macia, Nuria and Ho, Tin Kam},
#  journal={Universitat Ramon Llull, La Salle},
#  volume={196},
#  year={2010}
#}


branch <- function(data, j) {
    data[data$class == j, -ncol(data), drop=FALSE]
}


num <- function(data, j) {

    tmp = branch(data, j)
    aux = nrow(tmp) * (colMeans(tmp) - 
        colMeans(data[,-ncol(data)]))^2
    return(aux)
}


den <- function(data, j) {

    tmp = branch(data, j)
    aux = rowSums((t(tmp) - colMeans(tmp))^2)
    return(aux)
}


f1 <- function(data) {

    aux = mapply(function(j) {
        num(data, j)/den(data, j)
    }, j=levels(data$class))

    aux[aux == Inf] = NA
    aux = rowSums(aux, na.rm=TRUE)
    return(max(aux))
}


f1v <- function(data) {
    aux = predict(lda(class ~., data), data)
    data = data.frame(aux$x, class=data$class)
    f1(data)
}


regionOver <- function(data) {

    l = levels(data$class)
    a = branch(data, l[1])
    b = branch(data, l[2])

    maxmax = rbind(colMax(a), colMax(b))
    minmin = rbind(colMin(a), colMin(b))

    over = colMax(rbind(colMin(maxmax) - colMax(minmin), 0))
    rang = colMax(maxmax) - colMin(minmin)

    aux = prod(over/rang, na.rm=TRUE)
    return(aux)
}


f2 <- function(data) {

    data = ovo(data)
    aux = unlist(lapply(data, regionOver))
    return(mean(aux)) # sum is not comparable 
    # ??? take the maximum???
}


nonOverlap <- function(data) {

    l = levels(data$class)
    a = branch(data, l[1])
    b = branch(data, l[2])

    minmax = colMin(rbind(colMax(a), colMax(b)))
    maxmin = colMax(rbind(colMin(a), colMin(b)))

    aux = do.call("cbind",
        lapply(1:(ncol(data)-1), 
            function(i) {
                data[,i] < maxmin[i] | data[,i] > minmax[i]
        })
    )

    aux = data.frame(aux)
    rownames(aux) = rownames(data)
    return(aux)
}


f3 <- function(data) {

    data = ovo(data)
    aux = mapply(function(d) {
        colSums(nonOverlap(d))/nrow(d)
    }, d=data)

    aux = mean(colMax(aux)) # ??? take the average or maximum?
    return(aux)
}


removing <- function(data) {

    repeat {
        tmp = nonOverlap(data)
        col = which.max(colSums(tmp))
        aux = rownames(tmp[tmp[,col] != TRUE, , drop=FALSE])
        data = data[aux,- col, drop=FALSE]
        if(nrow(data) == 0 | ncol(data) == 1 |
            length(unique(data$class)) == 1)
            break
    }

    return(data)
}


f4 <- function(data) {

    data = ovo(data)
    aux = mapply(function(d) {
        n = removing(d)
        (nrow(d) - nrow(n))/nrow(d)
    }, d=data)

    aux = mean(aux)
    return(aux)
}


fisher <- function(data) {

    data = binarize(data)
    aux = lapply(OVERLAPPING, 
        function(i) {
            do.call(i, list(data))
    })

    aux = unlist(aux)
    names(aux) = OVERLAPPING
    return(aux)
}

