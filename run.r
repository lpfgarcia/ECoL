# R Code
# Run the experiment
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# Start the experiment for all datasets


setup <- function() {

	aux = list.files("measures/", recursive = TRUE, full.name = TRUE)
	for(i in aux)
		source(i)
}


run <- function(...) {

	lapply(files, function(file) {
		cat(basename(file), "\n")
			data = read.arff(file)
			print(complexity(data))
		cat("\n")
	})
}


setup()
aux = commandArgs(TRUE)
if(length(aux) == 0) {
	run()
} else {
	files = paste(DIR, "/database/", aux[1], sep="")
	run(files)
}

