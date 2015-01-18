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
    
  if (tofile)
    write(strOut, fname, append=TRUE)
  else
    print(strOut)
}
