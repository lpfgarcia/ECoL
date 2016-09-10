# R Code
# Overlapping Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Overlapping Measures


numf1 <- function(data, i, j) {

	aux = nrow(data[data$class == j,]) * 
	((mean(data[data$class == j,i]) - mean(data[,i]))^2)
	return(aux)
}


denf1 <- function(data, i, j) {

	aux = sum((data[data$class == j,i] - mean(data[data$class == j,i]))^2)
	return(aux)
}


f1 <- function(data) {

	aux = unlist(
		lapply(1:(ncol(data)-1), function(i) {

			tmp = unlist(
				lapply(levels(data$class), function(j){
					numf1(data, i, j)/
					denf1(data, i, j)
				})
			)

			sum(tmp)
		})
	)

	return(max(aux))
}


overlap <- function(data, i) {

	a = data[data$class == levels(data$class)[1],i]
	b = data[data$class == levels(data$class)[2],i]

	aux = min(c(max(a), max(b))) - max(c(min(a), min(b)))
	aux = max(c(0, aux))
	return(aux)
}


range <- function(data, i) {

	a = data[data$class == levels(data$class)[1],i]
	b = data[data$class == levels(data$class)[2],i]

	aux = max(c(max(a), max(b))) - min(c(min(a), min(b)))
	return(aux)
}


f2 <- function(data) {

	data = ovo(data);

	aux = lapply(data, function(tmp) {

		vet = unlist(
			lapply(1:(ncol(tmp)-1), function(i) {
				overlap(tmp, i)/range(tmp, i)
			})
		)

		prod(vet)
	})

	return(sum(aux))
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