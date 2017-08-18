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


eNN <- function(data, e=0.15) {

    dst = dist(data[,-ncol(data)])
    e = e*nrow(data)

    for(i in 1:nrow(dst)) {

        x = names(sort(dst[i,])[1:e+1])
        y = rownames(data[data$class == data[i,]$class,])
        dst[i,] = 0
        dst[i, intersect(x, y)] = 1
    }

    return(dst)
}


network <- function(data, e) {

    graph = eNN(data, e)
    graph = graph.adjacency(graph, mode="undirected")

    aux = lapply(NETWORK, 
        function(i) {
            do.call(i, list(graph))
    })

    aux = unlist(aux)
    names(aux) = NETWORK
    return(aux)
}

