chromotocycle <- function(strchromosome) {
  ## Transform an adjacency list representation into a cyclic representation.
  ## Input:  A chromosome "chromosome" containing n synteny blocks.
  ## Output: The sequence "nodes" of integers between 1 and 2n resulting from
  ##         applying chromotocyle to "chromosome."

  chromosome <- readP(strchromosome)
  
  chromlen <- length(chromosome)
  nodes <- vector(mode = 'numeric', length = 2*chromlen)
  
  for (j in 1:chromlen) {
    i <- chromosome[j]
    if (i > 0) {
      nodes[2*j - 1] <- ((2*i) - 1)
      nodes[2*j] <- 2 * i
    }
    else {
      nodes[2*j - 1] <- -2*i
      nodes[2*j] <- ((-2*i) - 1)
    }
  } ## end for (j in 1:chromlen)

  #printP(nodes, tofile=FALSE)
  printP(nodes, tofile=FALSE, showsign=FALSE)  

  return(nodes)
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
