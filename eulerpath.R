eulerpath <- function(adjlist) {
  ## Input: The adjacency list of an Eulerian directed graph.
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
  
  

  ## Create a data structure to hold nodes and edges and track which edges have
  ## been visited.



}



parseadjlist <- function(adjacencylist) {
  ## Helper function used to parse the adjacency list into a list of nodes and edges
  nodes <- vector('numeric')
  out <- vector('numeric')

  #result <- sapply(adjacencylist, strsplit, " -> ")
  result <- sapply(adjacencylist, strsplit, " -> ")
  print(result[[1]])

  for (i in 1:length(result)) {
    nodes[i] <- result[[i]][1]
  }
  
  return(result)
}
