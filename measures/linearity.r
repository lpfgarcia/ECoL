# R Code
# Measures of Linearity
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Linearity and Non Linearity Measures


smo <- function(data) {
	svm(class ~ ., data, scale=FALSE, kernel="linear")
}


l1 <- function(model, data) {

	aux = mapply(function(m, d) {
		prd = predict(m, d, decision.values=TRUE)
		err = rownames(d[prd != d$class,])
		dst = attr(prd, "decision.values")[err,]
		sum(abs(dst))/nrow(d)
	}, m=model, d=data)

	aux = max(aux)
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
	aux = filter(data, class == aux) %>% sample_n(., 2)

	for(i in 1:(ncol(data)-1)) {
		if(is.numeric(data[,i])) {
			rnd = runif(1)
			aux[1,i] = aux[1,i]*rnd + aux[2,i]*(1-rnd)
		}
	}
# ACL - retornando o exemplo 1 simplesmente, aí provavelmente o classificador vai acertá-lo, pois estará medindo o erro de treinamento. Se, por outro lado, escolher partes de cada exemplo (como um crossover uniforme), então gerará algo realmente novo
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


linearity <- function(data) {

	data = ovo(data)
	model = lapply(data, smo)

	aux = lapply(LINEARITY, 
		function(i) {
			do.call(i, list(model, data))
	})

	aux = unlist(aux)
	return(aux)
}

