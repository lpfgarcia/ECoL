# R Code
# Measures of Linearity
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Linearity and Non Linearity Measures


smo <- function(data) {
    svm(class ~ ., data, scale=TRUE, kernel="linear")
}


hyperretangle <- function(data) {
    data = data[,-ncol(data), drop=FALSE]
    aux = sqrt(sum((colMax(data)-colMin(data))^2))
    return(aux)
}


l1 <- function(model, data) {

    aux = mapply(function(m, d) {
        prd = predict(m, d, decision.values=TRUE)
        err = rownames(d[prd != d$class,])
        dst = attr(prd, "decision.values")[err,]
        sum(abs(dst))/(nrow(d)*hyperretangle(d))
    }, m=model, d=data)

    aux = mean(aux)
    return(aux)
}


error <- function(pred, class) {
    1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}


l2 <- function(model, data) {

    aux = mapply(function(m, d) {
        pred = predict(m, d)
        error(pred, d$class)
    }, m=model, d=data)

    return(mean(aux))
}


interpolation <- function(data) {

    aux = sample(levels(data$class), 1)
    aux = data[data$class == aux,] %>% sample_n(., 2)

    for(i in 1:(ncol(data)-1)) {
        if(is.numeric(data[,i])) {
            rnd = runif(1)
            aux[1,i] = aux[1,i]*rnd + aux[2,i]*(1-rnd)
        }
    }

    return(aux[1,])
}


generate <- function(data) {

    tmp = do.call("rbind",
        lapply(1:nrow(data),
            function(i) {
                interpolation(data)
        })
    )

    return(tmp)
}


l3 <- function(model, data) {

    aux = mapply(function(m, d) {
        tmp = generate(d)
        pred = predict(m, tmp)
        error(pred, tmp$class)
    }, m=model, d=data)

    return(mean(aux))
}


ls.linearity <- function() {
    c("l1", "l2", "l3")
}


linearity <- function(data) {

    data = binarize(data)

    data = ovo(data)
    model = lapply(data, smo)

    aux = lapply(ls.linearity(), 
        function(i) {
            do.call(i, list(model, data))
    })

    aux = unlist(aux)
    names(aux) = ls.linearity()
    return(aux)
}

