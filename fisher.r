# R Code
# Fisher Measures
# The set of Fisher measures


form <- function(data) {

	att = paste(colnames(data)[1:(ncol(data)-1)], collapse="+")
	aux = formula(paste("~ 0 +", att, sep=" "))
	return(aux)
}


preprocessing <- function(data) {

	tmp = model.matrix(form(data), data)
	tmp = data.frame(tmp, class=data$class)
	return(tmp);
}