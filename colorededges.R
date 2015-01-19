colorededges <- function(strP) {
  ## Input:   A genome P
  ## Output:  The collection of colored edges in the genome graph of P in the form (x, y).

  edges <- vector('character')
  strchroms <- splitchroms(strP)
  for (i in 1:length(strchroms)) {
    nam <- paste("P", i, sep="")
    assign(nam, strchroms[i])
  }

  for (i in 1:length(strchroms)) {
    chrom <- get(paste("P", i, sep=""))
    source("chromotocycle.R")
    nodes <- chromotocycle(chrom)
    nodes <- c(nodes, nodes[1])

    stopval <- length(nodes) / 2
    for (j in 1:stopval) {
      edges <- c(edges, paste("(", nodes[2*j], ", ", nodes[(2*j) + 1], ")", sep=""))
    } ## end for (j in 1:length(chrom))
    
  } ## end for (i in 1:length(strchroms))

  stredges <- paste(edges, "", sep=", ", collapse="")
  stredges <- substr(stredges, 1, nchar(stredges) - 2)

  return(stredges)
}



splitchroms <- function(strgenome) {
  ## Separate the chromosomes in a genome provided as a string.
  chroms <- strsplit(strgenome, ")")[[1]]
  chroms1 <- sapply(chroms, paste, ")", sep="")
  chroms1 <- unname(chroms1)
  ##print(chroms1)  
  return(chroms1)
}
