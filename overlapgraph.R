overlapgraph <- function(patterns) {
  ##
  numpatterns <- length(patterns)

  patterns <- sort(patterns)
  prefixes <- vector(mode='character', length=numpatterns)
  suffixes <- vector(mode='character', length=numpatterns)
  ##adjacencylisti <- vector('character')
  #adjacencylist <- list()
  #adjacencylist <- vector('character')
  adjacencylist <- vector(mode='list', length=numpatterns)
  graph <- as.data.frame(cbind(patterns, prefixes, suffixes, adjacencylist), stringsAsFactors=FALSE)

  for (i in 1:numpatterns) {
    ## For each pattern in patterns, get the prefix and suffix of the pattern, where
    ## the prefix is the first (k - 1) symbols of pattern, and the suffix is the last
    ## (k - 1) symbols of the pattern. e.g if pattern = "ATAT" then prefix = "ATA" and
    ## suffix = "TAT".
    ## graph$prefixes[i] <- getkminus1(patterns[i], "first")
    ## graph$suffixes[i] <- getkminus1(patterns[i], "last")
    graph$prefixes[[i]] <- getkminus1(patterns[i], "first")
    graph$suffixes[[i]] <- getkminus1(patterns[i], "last")    
  }

  ## Iterate through the patterns list again and find where each prefix matches each suffix.
  ## If they match, add the suffix of the matching pair to the adjacency list of the prefix.
  ## The length of prefixes and suffixes should be the same as the length of patterns, and
  ## I used the variable "numpatterns" to store the value (to save on repeated calls to
  ## length(patterns).
  for (i in 1:numpatterns) {
    #pre <- graph$prefixes[i]    
    #pre <- graph$prefixes[[i]]
    #print(pre)    
    suf <- graph$suffixes[[i]]
    print(suf)    


    temp <- vector('character')
    for (j in 1:numpatterns) {
      ##if (pre == graph$suffixes[j]) {
      ##if (pre == graph$suffixes[[j]]) {
      if (suf == graph$prefixes[[j]]) {                
        ## Add it to the adjacency list of patterns[i]
        ##print(paste("i=", i, "j=", j, "Prefix =", pre, "suffixes[j] =", graph$suffixes[j], sep=" "))
        #print(paste("i=", i, "j=", j, "Prefix =", pre, "suffixes[[j]] =", graph$suffixes[[j]], sep=" "))
        print(paste("i=", i, "j=", j, "prefixes[[j]] =", graph$prefixes[[j]], "Suffix =", suf, sep=" "))        
        ##graph$adjacencylisti[i] <- c(graph$adjacencylisti[i], graph$suffixes[j])
        #temp <- c(temp, graph$suffixes[j])
        temp <- c(temp, graph$patterns[j])
        
      } ## end if (pre == suffixes[j])
        
    } ## end for (j in 1:length(suffixes))

    ##graph$adjacencylist[i] <- c(graph$adjacencylist[i], temp)
    graph$adjacencylist[[i]] <- c(graph$adjacencylist[[i]], temp)    

  } ## end for (i in 1:numpatterns)
  
  return(graph)
}



printadjlist <- function(graph) {
  ## Input is a data frame (graph) which contains the patterns, prefixes, suffixes, and adjacency lists
  ## Output is a vector of strings of the format:
  ##      pattern_i -> adjacencylist_i
  ## Sorted by the "patterns" column.

  #Need to sort before creating the graph data frame, as one apparently
  ## cannot sort lists in R.
  #graph <- graph[order(graph$patterns)]

  for (i in 1:nrow(graph)) {
    ## Create a string for each pattern, mapping from pattern to adjacency list
    ## e.g. AGGCA -> GGCAT
    
  }

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










## overlapgraph <- function(patterns) {
##   ##
##   numpatterns <- length(patterns)
##   graph <- as.data.frame(patterns, stringsAsFactors=FALSE)
##   prefixes <- vector(mode='character', length=numpatterns)
##   suffixes <- vector(mode='character', length=numpatterns)
##   adjacencylist <- list()
##   adjacencylisti <- vector('character')

##   for (i in 1:numpatterns) {
##     ## For each pattern in patterns, get the prefix and suffix of the pattern, where
##     ## the prefix is the first (k - 1) symbols of pattern, and the suffix is the last
##     ## (k - 1) symbols of the pattern. e.g if pattern = "ATAT" then prefix = "ATA" and
##     ## suffix = "TAT".
    
##     prefixi <- getkminus1(patterns[i], "first")
##     suffixi <- getkminus1(patterns[i], "last")
##     prefixes[i] = prefixi
##     suffixes[i] = suffixi
##   }

##   ## Iterate through the patterns list again and find where each prefix matches each suffix.
##   ## If they match, add the suffix of the matching pair to the adjacency list of the prefix.
##   ## The length of prefixes and suffixes should be the same as the length of patterns, and
##   ## I used the variable "numpatterns" to store the value (to save on repeated calls to
##   ## length(patterns).
##   for (i in 1:numpatterns) {
##     pre <- prefixes[i]

##     for (j in 1:length(suffixes)) {  ## used length(suffixes) here just for greater clarity.
##       if (pre == suffixes[j]) {
##         ## Add it to the adjacency list of patterns[i]
##         adjacencylisti <- c(adjacencylisti, suffixes[j])
        
##       } ## end if (pre == suffixes[j])
        
##     } ## end for (j in 1:length(suffixes))

##     adjacencylist <- c(adjacencylist, adjacencylisti)
##   } ## end for (i in 1:numpatterns)
  
##   graph <- cbind(graph, prefixes, suffixes, stringsAsFactors=FALSE)

##   return(graph)
## }
