# R Code
# Overlapping Measures
# The set of Overlapping Measures


form <- function(data) {

	att = paste(colnames(data)[1:(ncol(data)-1)], collapse="+")
	aux = formula(paste("~ 0 +", att, sep=" "))
	return(aux)
}


preprocessing <- function(data) {

	aux = model.matrix(form(data), data)
	aux = data.frame(aux, class=data$class)
	return(aux);
}


f1 <- function(data) {

	lapply(1:(ncol(data)-1), function(i) {
			
			lapply(levels(data$class), function(j){

			})

	}


}









fisher <- function(data) {

	data = preprocessing(data)

	aux = lapply(
		c("f1", "f2", "f3", "f4"), function(i) {
			do.call(i, list(data))
		}
	)

	aux = unlist(aux)
	return(aux)
}	


}