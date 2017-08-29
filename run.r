# R Code
# Run the experiment
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# Start the experiment for all datasets

require(cluster)
require(dplyr)
require(e1071)
require(foreign)
require(igraph)
require(MASS)


setup <- function() {

    aux = list.files("measures/", recursive=TRUE, full.name=TRUE)
    for(i in aux)
        source(i)
}


setup()

