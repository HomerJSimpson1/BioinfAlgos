debruijncomp <- function(patterns) {
  ## Solve the DeBruijn Graph from a string Problem

  ## patterns <- composition(k, text)
  ## #print(patterns)
  
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
  #return(uniquepresuf)

  ## Now walk the "patterns" vector and connect prefixes with suffixes to form the adjacency lists
  for (i in 1:numpatterns) {
    
  }
  

  

  ## adjacencylist <- vector(mode='list', length=numpatterns)
  
  ## ## Iterate through the patterns list again and find where each prefix matches each suffix.
  ## ## If they match, add the suffix of the matching pair to the adjacency list of the prefix.
  ## ## The length of prefixes and suffixes should be the same as the length of patterns, and
  ## ## I used the variable "numpatterns" to store the value (to save on repeated calls to
  ## ## length(patterns).
  ## for (i in 1:numpatterns) {
  ##   suf <- suffixes[i]    
  ##   temp <- vector('character')
  ##   for (j in 1:numpatterns) {
  ##     if (suf == prefixes[j]) {        
  ##       ## Add it to the adjacency list of patterns[i]
  ##       #temp <- c(temp, patterns[j]) ## From the Hamiltonian Graph (nodes are patterns)
  ##       temp <- c(temp, prefixes[j])  ## Debruijn Graph, nodes are prefixes and suffixes
  ##     } ## end if (pre == suffixes[j])        
  ##   } ## end for (j in 1:length(suffixes))


  ##   adjacencylist[[i]] <- c(adjacencylist[[i]], temp)
    
  ## } ## end for (i in 1:numpatterns)

  ## removelist <- vector('numeric')
  ## #print(paste(adjacencylist, "", sep="", collapse=", "))  

  ## ## Iterate through the prefixes and find all prefixes that are the same
  ## ## If two prefixes are equal, then merge their adjacency lists.
  ## for (i in 1:(numpatterns - 1)) {
  ##   for (j in (i + 1):numpatterns) {
  ##     ##print(paste("i=",i,"j=",j,sep=" "))
  ##     ##print(paste("patterns[i]=", patterns[i], "patterns[j]=",patterns[j], sep=" "))
  ##     #if (patterns[i] == patterns[j]) {  ## Duplicates prefixes, NOT duplicate patterns
  ##     if (prefixes[i] == prefixes[j]) {
  ##       ## Then merge their adjacency lists
  ##       adjacencylist[[i]] <- c(adjacencylist[[i]], adjacencylist[[j]])
  ##       ## Updating the suffixes should be unnecessary, as they are captured in the adjacency lists.
  ##       #suffixes[i] <- c(suffixes[i], suffixes[j])
  ##       ##print(adjacencylist[[i]])

  ##       ## Add j to the list of elements to remove
  ##       removelist <- c(removelist, j)
        
  ##       ## ## Remove pattern[j], prefix[j], suffix[j], and adjacencylist[[j]]
  ##       ## print(paste("i=",i,"j=",j,sep=" "))
  ##       ## patterns <- patterns[-j]
  ##       ## prefixes <- prefixes[-j]
  ##       ## suffixes <- suffixes[-j]
  ##       ## adjacencylist <- adjacencylist[-j]

  ##       ## print(patterns)
  ##       ## print(prefixes)
  ##       ## print(suffixes)
  ##       ## print(adjacencylist)
                             
        
  ##       ## Kill the following for now, because we actually need repeated elements sometimes.
  ##       ## temp <- c(adjacencylist[[i]], adjacencylist[[j]])
  ##       ## templen <- length(temp)
  ##       ## for (k in 1:templen)
  ##       ##   for (l in (k+1):templen)
        
  ##       ## for (k in 1:length(adjacencylist[[i]])) {
  ##       ##   for (l in 1:length(adjacencylist[[j]]) {
  ##       ##     if (adjacencylist[[i]][k] != adjacencylist[[j]][l]) {
  ##       ##       adjacencylist[[i]] <- c(adjacencylist[[i]], adjacencylist[[j]][l])
  ##       ##     }
  ##       ##   }
  ##       ## } ## end for (k in 1:length(adjacencylist[[i]]))

        
  ##     } ## end if(patterns[i] == patterns[j])
  ##   } ## end for (j in 1:numpatterns)
  ## } ## end for (i in 1:numpatterns)


  ## ## Remove all elements in the "remove list" from patterns, prefixes, suffixes, and adjacencylist
  ## ## Actually, do NOT remove the patterns.  These are the edges and should remain intact.
  ## ## patterns <- patterns[-removelist]
  ## prefixes <- prefixes[-removelist]
  ## suffixes <- suffixes[-removelist]
  ## adjacencylist <- adjacencylist[-removelist]

  ## #print(patterns)
  ## #print(prefixes)
  ## #print(suffixes)
  ## #print(adjacencylist)
  ## #print(paste(adjacencylist, "", sep="", collapse=", "))

  ## ## Can no longer include the patterns in the data frame, as the patterns list is now longer than the
  ## ## other lists.  :-(
  ## ## graph <- as.data.frame(cbind(patterns, prefixes, suffixes, adjacencylist), stringsAsFactors=FALSE)  
  ## graph <- as.data.frame(cbind(prefixes, suffixes, adjacencylist), stringsAsFactors=FALSE)  

  ## outstring <- ""
  ## outstring <- printdebgraph(graph)
  ## print(outstring)
  ## writestringtofile("debruijnout.txt", outstring)
  ## return(graph)
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

  #output <- ""
  output <- NULL
  for (i in 1:nrow(graphdf)) {
    if (length(graphdf$adjacencylist[[i]]) > 0) {
      #output <- paste(output, graphdf$prefixes[i], " -> ", paste(graphdf$adjacencylist[[i]], "",sep="",collapse=", "), "\n", sep="")
      output <- paste(output, graphdf$prefixes[i], " -> ", paste(graphdf$adjacencylist[[i]], "",sep="",collapse=","), "\n", sep="")      
      #str(output)
      #print(output)
    }
  }
  
  return(output)
}




## ## For the Hamiltonian Graph:
## printgraph <- function(graphdf) {
##   ## Print the graph adjacency list in the graphdf data frame, using the adjacency list column.
##   ## Print in the form of, pattern -> adjacency list
##   ## For some reason, this seems to produce an extra line with a space on it, so after running
##   ## and saving the resulting string to a file, you have to open the file, go to the last line
##   ## of the file, and remove the second to last line (consisting of just a space character).

##   output <- ""
##   for (i in 1:nrow(graphdf)) {
##     if (length(graphdf$adjacencylist[[i]]) > 0) {
##       output <- paste(output, graphdf$patterns[i], " -> ", graphdf$adjacencylist[[i]], "\n", sep="")
##     }
##   }
  
##   return(output)
## }



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
  #print(filename)
  if (is.vector(string))
    {
      #string <- paste(string, "", sep=" ", collapse="")
      string <- paste(string, "", sep="", collapse=" ")      
      #print(string)
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
  #print(sequence)
  ## For the remainder of the patterns vector, add the last symbol of each pattern
  for (i in 2:length(patterns)) {
    #print(patterns[i])
    sequence <- c(sequence, lastsymbol(patterns[i]))
  }

  ## Collapse into a string
  sequence <- paste(sequence, "", sep="", collapse="")

  ## Return the result
  return(sequence)
}












## debruijn <- function(k, text) {
##   ## Solve the DeBruijn Graph from a string Problem
  
##   patterns <- composition(k, text)
##   #print(patterns)
  
##   numpatterns <- length(patterns)

##   patterns <- sort(patterns)
##   prefixes <- vector(mode='character', length=numpatterns)
##   suffixes <- vector(mode='character', length=numpatterns)

##   for (i in 1:numpatterns) {
##     ## For each pattern in patterns, get the prefix and suffix of the pattern, where
##     ## the prefix is the first (k - 1) symbols of pattern, and the suffix is the last
##     ## (k - 1) symbols of the pattern. e.g if pattern = "ATAT" then prefix = "ATA" and
##     ## suffix = "TAT".
##     prefixes[i] <- getkminus1(patterns[i], "first")
##     suffixes[i] <- getkminus1(patterns[i], "last")    
##   }

##   adjacencylist <- vector(mode='list', length=numpatterns)
  
##   ## Iterate through the patterns list again and find where each prefix matches each suffix.
##   ## If they match, add the suffix of the matching pair to the adjacency list of the prefix.
##   ## The length of prefixes and suffixes should be the same as the length of patterns, and
##   ## I used the variable "numpatterns" to store the value (to save on repeated calls to
##   ## length(patterns).
##   for (i in 1:numpatterns) {
##     ## For this problem, we are given text, so use that to create the adjacency list
##     ## Just run down the suffix list and add the ith suffix to the ith adjacency list
##     adjacencylist[[i]] <- suffixes[i]


##     ## Previous strategy used in overlapgraph.R, where we added each suffix that matched the prefix
##     ## (actually, in overlapgraph.R, we added patterns to the adjacency list, not suffixes).
##     ## suf <- suffixes[i]    
##     ## temp <- vector('character')
##     ## for (j in 1:numpatterns) {
##     ##   if (suf == prefixes[j]) {        
##     ##     ## Add it to the adjacency list of patterns[i]
##     ##     #temp <- c(temp, patterns[j]) ## From the Hamiltonian Graph (nodes are patterns)
##     ##     temp <- c(temp, prefixes[j])  ## Debruijn Graph, nodes are prefixes and suffixes
##     ##   } ## end if (pre == suffixes[j])        
##     ## } ## end for (j in 1:length(suffixes))

##     ## adjacencylist[[i]] <- c(adjacencylist[[i]], temp)
    
##   } ## end for (i in 1:numpatterns)

##   removelist <- vector('numeric')
##   #print(paste(adjacencylist, "", sep="", collapse=", "))  

##   ## Iterate through the prefixes and find all prefixes that are the same
##   ## If two prefixes are equal, then merge their adjacency lists.
##   for (i in 1:(numpatterns - 1)) {
##     for (j in (i + 1):numpatterns) {
##       ##print(paste("i=",i,"j=",j,sep=" "))
##       ##print(paste("patterns[i]=", patterns[i], "patterns[j]=",patterns[j], sep=" "))
##       #if (patterns[i] == patterns[j]) {  ## Duplicates prefixes, NOT duplicate patterns
##       if (prefixes[i] == prefixes[j]) {
##         ## Then merge their adjacency lists
##         adjacencylist[[i]] <- c(adjacencylist[[i]], adjacencylist[[j]])
##         ## Updating the suffixes should be unnecessary, as they are captured in the adjacency lists.
##         #suffixes[i] <- c(suffixes[i], suffixes[j])
##         ##print(adjacencylist[[i]])

##         ## Add j to the list of elements to remove
##         removelist <- c(removelist, j)
        
##         ## ## Remove pattern[j], prefix[j], suffix[j], and adjacencylist[[j]]
##         ## print(paste("i=",i,"j=",j,sep=" "))
##         ## patterns <- patterns[-j]
##         ## prefixes <- prefixes[-j]
##         ## suffixes <- suffixes[-j]
##         ## adjacencylist <- adjacencylist[-j]

##         ## print(patterns)
##         ## print(prefixes)
##         ## print(suffixes)
##         ## print(adjacencylist)
                             
        
##         ## Kill the following for now, because we actually need repeated elements sometimes.
##         ## temp <- c(adjacencylist[[i]], adjacencylist[[j]])
##         ## templen <- length(temp)
##         ## for (k in 1:templen)
##         ##   for (l in (k+1):templen)
        
##         ## for (k in 1:length(adjacencylist[[i]])) {
##         ##   for (l in 1:length(adjacencylist[[j]]) {
##         ##     if (adjacencylist[[i]][k] != adjacencylist[[j]][l]) {
##         ##       adjacencylist[[i]] <- c(adjacencylist[[i]], adjacencylist[[j]][l])
##         ##     }
##         ##   }
##         ## } ## end for (k in 1:length(adjacencylist[[i]]))

        
##       } ## end if(patterns[i] == patterns[j])
##     } ## end for (j in 1:numpatterns)
##   } ## end for (i in 1:numpatterns)


##   ## Remove all elements in the "remove list" from patterns, prefixes, suffixes, and adjacencylist
##   ## Actually, do NOT remove the patterns.  These are the edges and should remain intact.
##   ## patterns <- patterns[-removelist]
##   prefixes <- prefixes[-removelist]
##   suffixes <- suffixes[-removelist]
##   adjacencylist <- adjacencylist[-removelist]

##   #print(patterns)
##   #print(prefixes)
##   #print(suffixes)
##   #print(adjacencylist)
##   #print(paste(adjacencylist, "", sep="", collapse=", "))

##   ## Can no longer include the patterns in the data frame, as the patterns list is now longer than the
##   ## other lists.  :-(
##   ## graph <- as.data.frame(cbind(patterns, prefixes, suffixes, adjacencylist), stringsAsFactors=FALSE)  
##   graph <- as.data.frame(cbind(prefixes, suffixes, adjacencylist), stringsAsFactors=FALSE)  

##   outstring <- ""
##   outstring <- printdebgraph(graph)
##   print(outstring)
##   writestringtofile("debruijnout.txt", outstring)
##   return(graph)
## }
