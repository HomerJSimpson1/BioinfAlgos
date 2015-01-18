colorededges0 <- function(strP) {
  ## Input:   A genome P
  ## Output:  The collection of colored edges in the genome graph of P in the form (x, y).

  #P <- readP(strP)
  edges <- vector('character')
  strchroms <- splitchroms(strP)
  for (i in 1:length(strchroms)) {
    nam <- paste("P", i, sep="")
    assign(nam, strchroms[i])
    #nam <- as.integer(strsplit(nam, " ")[[1]])
    
    #temp <- as.integer(strsplit(substr(input, 2, nchar(input) - 1), " ")[[1]])  )
    #nam
    #print(nam)
  }
  #P <- as.integer(Pvec)
  
##   chromlen <- length(P)
##   edges <- vector(mode='numeric', length=((2*chromlen) + 1))
  #return(P)
  #print(P2)
  #print(as.integer(strsplit(P2, " ")[[1]]))
  #val <- as.integer(strsplit(P2, " ")[[1]])
  #print(val)
  
  #return(P1)

  for (i in 1:length(strchroms)) {
    chrom <- get(paste("P", i, sep=""))
    ## Convert the string vector to an integer vector
    #chrom <- as.integer(strsplit(get(paste("P", i, sep="")), " ")[[1]])
    #print(chrom)
    #(require "chromotocycle.R")
    source("chromotocycle.R")
    nodes <- chromotocycle(chrom)
    
##     for (j in 1:length(chrom)) {
##       edges <- c(edges, paste("(", nodes[2*j], " ", nodes[(2*j) + 1], ")", sep=""))
##     } ## end for (j in 1:length(chrom))
    
  } ## end for (i in 1:length(strchroms))

  return(edges)
}



splitchroms <- function(strgenome) {
  ## Separate the chromosomes in a genome provided as a string.
  chroms <- strsplit(strgenome, ")")[[1]]
  #chroms1 <- sapply(chroms, substr, 2, nchar(chroms))
  chroms1 <- sapply(chroms, paste, ")", sep="")
  chroms1 <- unname(chroms1)
  #chroms1 <- vector('character', length(chroms))
  #for (i in 1:length(chroms)) {
  #  chroms1[i] <- substr(chroms, 2, nchar(chroms[i]))
  #}
  
  return(chroms1)
}
