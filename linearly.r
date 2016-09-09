# R Code
# Linear Measures
# The set of linear measures

require(dplyr)
require(e1071)

acc <- function(pred, class) {

	tb = 1 - sum(diag(table(class, pred)))/
		sum(table(class, pred))

	return(tb)
}


ovo <- function(data) {

	aux = combn(levels(data$class), 2)

	tmp = lapply(1:ncol(aux), function(i) {
		vet = subset(data, data$class %in% aux[,i])
		vet$class = factor(vet$class, labels=1:2)
		return(vet)
	})

	return(tmp)
}


interpolation <- function(data) {

	aux = sample(levels(data$class), 1)
	aux = filter(data, class == aux) %>% sample_n(., 2)
	tmp = aux[1,]

	for(i in 1:(ncol(data)-1)) {
		if(is.factor(data[,i])) {
			tmp[1,i] = sample(aux[,i], 1)
		} else {
			rnd = runif(1)
			tmp[1,i] = aux[1,i]*rnd + aux[2,i]*(1-rnd)
		}
	}

	return(tmp)
}


l1 <- function(data, model) {

	aux = mapply(function(m, d){
		prd = predict(m, d, decision.values=TRUE)
		err = rownames(d[prd != d$class,])
		dst = attr(prd,"decision.values")[err,]
		sum(abs(dst))
	}, m=model, d=data)

	aux = max(aux)
	return(aux)
}


l2 <- function(data, model) {

	aux = mapply(function(m, d) {
		pred = predict(m, d)
		acc(pred, d$class)
	}, m=model, d=data)

	return(mean(aux))
}


l3 <- function(data, model) {

	aux = mapply(function(m, d) {

		tmp = do.call("rbind",
			lapply(1:nrow(d), function(i) {
				interpolation(d)
			})
		)

		tmp = data.frame(tmp)
		tmp$class = factor(tmp$class)
		pred = predict(m, tmp)
		acc(pred, tmp$class)
	}, m=model, d=data)

	return(mean(aux))
}


linearly <- function(data) {

	data = ovo(data);
	model = lapply(data, function(tmp) {
		svm(class ~ ., tmp, kernel="linear")
	})

	aux = lapply(c("l1", "l2", "l3"), 
		function(i) {
			do.call(i, list(data, model))
		}
	)

	aux = unlist(aux)
	return(aux);
}

