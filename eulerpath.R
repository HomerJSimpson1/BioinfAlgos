#eulerpath <- function(adjlist) {
eulerpath <- function(fname="eulersampleinput.txt") {  
  ## Input: The adjacency list of an Eulerian directed graph. The
  ## adjacency list is assumed to be saved in a file whose name
  ## is given by the input parameter "fname."
  ## Output: An Eulerian cycle in this graph

  ## Sample Input:
  ##      0 -> 3
  ##      1 -> 0
  ##      2 -> 1,6
  ##      3 -> 2
  ##      4 -> 2
  ##      5 -> 4
  ##      6 -> 5,8
  ##      7 -> 9
  ##      8 -> 7
  ##      9 -> 6
  
  ## Sample Output:
  ##      6->8->7->9->6->5->4->2->1->0->3->2->6


  ## Call helper function to parse the input adjacency list.
  df <- parseadjlist(fname)
  nodevec <- unlist(df$nodes)

  ##visited <- vector('logical', length=nrow(df))
  #visited <- rep(FALSE, nrow(df))

  ## Create a data structure to hold nodes and edges and track which edges have
  ## been visited.
  #df <- cbind(df, nodevec, visited)
##   df <- cbind(nodevec, df, visited)
##   df <- cbind(nodevec, df)  
##  numnodes <- nrow(df)
  numnodes <- length(nodevec)

  edges <- cbind(vector('numeric'), vector('numeric'), vector('logical'))

  for (i in 1:numnodes) {
    for (j in 1:length(df$adjlist[[i]])) {
      #print(length(df$adjlist[[i]]))
      edgerow <- c(df$nodevec[i], df$adjlist[[i]][j], FALSE)
      edgerow <- c(nodevec[i], df$adjlist[[i]][j], FALSE)      
      edges <- rbind(edges, edgerow)
    }
  }
  #print(edges)

  colnames(edges) <- c('Node', 'Out', 'Visited')
  numedges <- nrow(edges)
  print(numedges)
  #print(edges)

  ## currentnode holds the randomly selected start node.
  currentnode <- runif(1:numnodes)

  while(nrow(edges[edges$visited==FALSE,]) > 0) {
    ## While there are edges that have not yet been visited...
    currentnodeout <- edges[edges$node==currentnode,]
    
    if(nrow(currentnodeout[currentnodeout$visited==FALSE]) > 0) {
      ## if there are unvisited outbound edges from the node
      currentnode <- currentnodeout$Out
    
    } ## end if(nrow(currentnodeout...))
    else {
      ## then there are no unvisited outbound edges from the
      ## current node
      
    }
    
    
    
  } ## end while(nrow(edges...))

  #return(df)
  return(edges)
}



## getnextedge <- function(edges, node) {
##   ## For a given node, find the next edge that hasn't been visited.
##   ## This assumes that the nodes are ordered (either numerically or
##   ## lexicographically).
  
##   while((edges$node[] == node) && (edges$visited == TRUE)) {
    

##   }
## }

  


##parseadjlist <- function(adjacencylist) {
parseadjlist <- function(filename="eulersampleinput.txt") {  
  ## Helper function used to parse the adjacency list into a list of nodes and edges

  adjacencylist <- readfiletostring(filename, nocollapse=TRUE)

  #print(length(adjacencylist))
  
  #nodes <- vector('numeric')
  nodes <- vector('numeric', length=length(adjacencylist))  
  out <- vector('numeric')
  adjlist <- vector('character')

  #result <- sapply(adjacencylist, strsplit, " -> ")
  result <- sapply(adjacencylist, strsplit, " -> ")
  #print(result[[1]])

  for (i in 1:length(result)) {
    #nodes[i] <- result[[i]][1]
    nodes[i] <- as.numeric(result[[i]][1])
    #adjlist[i] <- result[[i]][2]
    adjlist[i] <- strsplit(result[[i]][2], ",")
    #adjlist[i] <- sub("[[:space:]]+$","",adjlist[i])
    #adjlist[i] <- as.numeric(strsplit(result[[i]][2], ",")[[i]])
  }

  #nodes <- as.numeric(nodes)
  #print(nodes)
  #print(adjlist)
  
  #adj <- sapply(adjlist, strsplit, ",")
  #print(adj)

  for (i in 1:length(adjlist)) {
    #adj[[i]] <- as.numeric(adj[[i]])
    #adjlist[[i]] <- as.numeric(adjlist[[i]])
    adjlist[[i]] <- as.numeric(sub("[[:space:]]+$","",adjlist[[i]]))
  }

  #print(adjlist)
  #print(adj)

  
  df <- as.data.frame(cbind(nodes, adjlist), stringsAsFactors=FALSE)
  #df <- as.data.frame(cbind(nodes, adjlist), optional=TRUE, stringsAsFactors=FALSE)  
  return(df)
}





## Functions for reading in files
readfiletostring <- function(filename, usespace=FALSE, nocollapse=FALSE) {
  ## Read the contents of a file into a single string.
  if(usespace)
    if(nocollapse)
      result <- paste(readLines(filename))
    else
      result <- paste(readLines(filename), collapse=" ")
  else
    if(nocollapse)
      result <- paste(readLines(filename))
    else
      result <- paste(readLines(filename), collapse="")

  return(result)
}




readfiletovec <- function(filename, header=FALSE, stringsAsFactors=FALSE) {
  ## Read the contents of a file into a vector
  ## Each line in the file corresponds to a single element of the vector.
  vec <- read.table(filename, header=header, stringsAsFactors=stringsAsFactors)
  return(vec[,1])
}
