# R Code
# Measures based on Graph
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Measues based on graph


edges <- function(graph) {
    ecount(graph)
}


avg_degree <- function(graph) {
    mean(degree(graph))
}


density <- function(graph) {
    graph.density(graph)
}


max_componet <- function(graph) {
    max(clusters(graph)$csize)
}


avg_closeness <- function(graph) {
    mean(closeness(graph))
}


avg_betweenness <- function(graph) {
    mean(betweenness(graph, directed=FALSE))
}


avg_hub <- function(graph) {
    mean(hub.score(graph)$vector)
}


cluster_coefficient <- function(graph) {
    transitivity(graph, type="global")
}


avg_path_length <- function(graph) {

    cls = clusters(graph)
    g = induced.subgraph(graph, which(cls$membership == which.max(cls$csize)))
    disthist = path.length.hist(g, directed=FALSE)$res
    aux = weighted.mean(1:length(disthist), disthist)  
    return(aux)
}


eNN <- function(data, epson=0.15) {

    dst = dist(data[,-ncol(data)])
    aux = matrix(0, nrow(data), nrow(data), dimnames=list(rownames(data), rownames(data)))

    for(i in 1:nrow(aux)) {
        
        x = names(sort(dst[i,])[1:(epson*nrow(data))+1])
        y = rownames(data[data$class == data[i,]$class,])
        z = intersect(x, y)
        aux[i, z] = 1
    }

    return(aux)
}


network <- function(data, epson) {

    mat = eNN(data, epson)
    graph = graph.adjacency(mat, mode="undirected")

    aux = lapply(NETWORK, 
        function(i) {
            do.call(i, list(graph))
    })

    aux = unlist(aux)
    names(aux) = NETWORK
    return(aux)
}

