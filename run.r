# R Code
# Run the experiment
# L. P. F. Garcia, A. C. P. L. F. de Carvalho, A. C. Lorena 2016
# Start the experiment for a specific or all datasets


setup <- function() {

	aux = list.files("measures/", recursive=TRUE, full.name=TRUE);
	for(i in aux)
		source(i);
}


run <- function(...) {

	lapply(files, function(file) {
		cat(basename(file), "\n"); 
			complexity(file); 
		cat("\n");
	});
}


setup();
aux = commandArgs(TRUE);
if(length(aux) == 0) {
	run();
} else {
	files = paste(DIR, "/database/", aux[1], sep="");
	run(files);
}

