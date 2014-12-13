debruijncomp <- function(patterns) {
  ## Solve the DeBruijn Graph from a string Problem

  numpatterns <- length(patterns)

  patterns <- sort(patterns)
  prefixes <- vector(mode='character', length=numpatterns)
  suffixes <- vector(mode='character', length=numpatterns)

  for (i in 1:numpatterns) {
    ## For each pattern in patterns, get the prefix and suffix of the pattern, where
    ## the prefix is the first (k - 1) symbols of pattern, and the suffix is the last
    ## (k - 1) symbols of the pattern. e.g if pattern = "ATAT" then prefix = "ATA" and
    ## suffix = "TAT".
    prefixes[i] <- getkminus1(patterns[i], "first")
    suffixes[i] <- getkminus1(patterns[i], "last")    
  }

  ## Create a vector that combines all unique prefixes and suffixes
  uniquepresuf <- unique(c(prefixes, suffixes))

  ## Now walk the "patterns" vector and connect prefixes with suffixes to form the adjacency lists
  adjlist <- vector(mode='list', length=length(uniquepresuf))
  
  for (i in 1:length(uniquepresuf)) {
    temp <- vector('character') 
    for (j in 1:numpatterns) {
      if (uniquepresuf[i] == prefixes[j]) {
        temp <- c(temp, suffixes[j])
      }
    }
    adjlist[[i]] <- c(adjlist[[i]], temp)
  }

  graph <- as.data.frame(cbind(uniquepresuf,  adjlist), stringsAsFactors=FALSE)  

  outstring <- NULL  
  outstring <- printdebgraph(graph)
  print(outstring)
  writestringtofile("debruijncompout.txt", outstring)
  return(graph)  
}




composition <- function(k, text) {
  ## Split the given text into all possible strings of length k
  ## and return the fragments in lexicographic order.
  textlen <- nchar(text)
  fragments <- character()
  
  for (i in 1:(textlen - k + 1)) {
    nextstr <- substr(text, i, i+k-1)
    fragments <- append(fragments, nextstr)
  }
  
  return(sort(fragments))
}




## For the Debruijn Graph:
printdebgraph <- function(graphdf) {
  ## Print the graph adjacency list in the graphdf data frame, using the adjacency list column.
  ## Print in the form of, prefix -> adjacency list
  ## For some reason, this seems to produce an extra line with a space on it, so after running
  ## and saving the resulting string to a file, you have to open the file, go to the last line
  ## of the file, and remove the second to last line (consisting of just a space character).

  output <- NULL
  for (i in 1:nrow(graphdf)) {
    if (length(graphdf$adjlist[[i]]) > 0) {      
      output <- paste(output, graphdf$uniquepresuf[i], " -> ", paste(graphdf$adjlist[[i]], "",sep="",collapse=","), "\n", sep="")                  
    }
  }
  
  return(output)
}




lastsymbol <- function(pattern) {
  ## Return the last symbol of pattern
  return(substr(pattern, nchar(pattern), nchar(pattern)))
}



getkminus1 <- function(pattern, firstorlast="first") {
  ## Get either the first k-1 symbols or the last k-1 symbols from pattern and return.

  k <- nchar(pattern)
  
  if(firstorlast == "first")
    result <- substr(pattern, 1, k - 1)
  else if(firstorlast == "last")
    result <- substr(pattern, 2, k)
  else
    stop("That is not a valid choice for first or last. Please choose exactly one of 'first' or 'last'.")

  return(result)
}




writestringtofile <- function(filename, string) {
  ## Write a string to a file given by filename
  if (is.vector(string))
    {
      string <- paste(string, "", sep="", collapse=" ")      
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)

}




stringgenomepath <- function(patterns) {
  ## Input:  A sequence of k-mers Pattern1, бн ,Patternn such that the last k - 1
  ##         symbols of Patterni are equal to the first k-1 symbols of Patterni+1
  ##         for 1 <= i <= n-1.
  ## Output: A string Text of length k+n-1 such that the i-th k-mer in Text is equal
  ##         to Patterni  (for 1 <= i <= n)

  ## Add the first element of the patterns vector
  sequence <- patterns[1]

  ## For the remainder of the patterns vector, add the last symbol of each pattern
  for (i in 2:length(patterns)) {
    sequence <- c(sequence, lastsymbol(patterns[i]))
  }

  ## Collapse into a string
  sequence <- paste(sequence, "", sep="", collapse="")

  ## Return the result
  return(sequence)
}
