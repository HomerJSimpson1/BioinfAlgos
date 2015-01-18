graphtogenome0 <- function(genomegraph) {
  ## Input:  The colored edges ColoredEdges of a genome graph.
  ## Output: The genome P corresponding to this genome graph.

  #P <- vector('character')
  nodes <- vector('numeric')
  genomevec <- splitchroms(genomegraph)
  mylist <- list()
  
  for (i in 1:length(genomevec)) {
    item <- genomevec[i]
    items <- strsplit(item, ",")[[1]]

    #items <- gsub('(', "", items)    
    #items <- gsub('\)', "", items)
    items <- as.integer(gsub("[[:punct:]]", "", items))
    #print(items)
    nodes <- c(nodes, items[1], items[2])
    if (items[1] > items[2]) {
      ## Then we've hit the end of the cycle
      nodes <- c(nodes[length(nodes)], nodes[1:(length(nodes) - 1)])
      mylist <- c(mylist, nodes)
      #print(nodes)
      strnodes <- paste(nodes, "", sep=" ", collapse="")
      strnodes <- substr(strnodes, 1, nchar(strnodes) - 1)
      strnodes <- paste("(", strnodes, ")", sep="")      
      ##strnodes <- paste("(", paste(nodes, "", sep=" ", collapse=""), ")", sep="")
      #print(strnodes)
      chromo <- cycletochromo(strnodes)
      chro <- printP(chromo, tofile=FALSE, showsign=TRUE)
      #print(chro)
      #P <- c(P, chromo)
      P <- paste(P, chro, sep=", ")
      nodes <- vector('numeric')
    } ## end if (items[1] > items[2])

  } ## end for (i in 1:length(genomevec))
  
  return(P)
}





splitchroms <- function(strgenome) {
  ## Separate the chromosomes in a genome provided as a string.
  chroms <- strsplit(strgenome, "), ")[[1]]
  #chroms <- substr(chroms, 1, nchar(chroms) - 1)
  chroms1 <- sapply(chroms, paste, ")", sep="")
  chroms1[length(chroms1)] <- substr(chroms1[length(chroms1)], 1, nchar(chroms1[length(chroms1)]) - 1)
  chroms1 <- unname(chroms1)
  
  return(chroms1)
}




cycletochromo <- function(strnodes) {
  ## Transform a cyclic representation into an adjacency list representation.
  ## Input:  A sequence Nodes of integers between 1 and 2n.
  ## Output: The chromosome Chromosome containing n synteny blocks resulting from
  ##         applying cyletochromo to "nodes."

  nodes <- readP(strnodes)  
  chromlen <- length(nodes) / 2
  chromo <- vector(mode = 'numeric', length = chromlen)
  
  for (j in 1:chromlen) {
    if (nodes[(2*j) - 1] < nodes[2*j])
      chromo[j] <- nodes[2*j] / 2
    else
      chromo[j] <- -(nodes[((2*j) - 1)] / 2)
  } ## end for (j in 1:chromlen)

  printP(chromo, tofile=FALSE, showsign=TRUE)

  return(chromo)
}




readP <- function(input) {
  ## Convert input into a vector
  temp <- as.integer(strsplit(substr(input, 2, nchar(input) - 1), " ")[[1]])  
  return(temp)
}




printP <- function(P, fname="", tofile=TRUE, showsign=TRUE) {
  ## Print the P vectorlines to a file
  if(showsign) {
    nums <- paste(sprintf("%+d", P), "", collapse="")
    trimd <- substr(nums, 1, nchar(nums) - 1)
  }
  else {
    nums <- paste(sprintf("%d", P), "", collapse="")
    trimd <- substr(nums, 1, nchar(nums) - 1)
  }
  strOut <- paste("(", trimd, ")", sep="")
    
##   if (tofile)
##     write(strOut, fname, append=TRUE)
##   else
##     print(strOut)
}
