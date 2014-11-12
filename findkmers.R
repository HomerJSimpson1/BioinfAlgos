findkmers <- function(string, k, L, t) {
  ## Find the number of clumps t of length k occurring within a window of length L in the long sequence "string".
  ## If the number of clumps == t, then we have a (L, t) clump of kmers.

  #result <- FALSE
  result <- vector('character')
  kmerstested <- vector('character')
  kmerstestednummatches <- vector('numeric')
  kmersmax <- vector('character')
  maxmatches = 0
  
  for(j in 1:(nchar(string)/L + 1))
    {
    window <- substring(string, (j-1)*L + 1, j*L + k)
    #print(window)
    for(i in 1:L+k)
      {
        posskmer <- substring(window, i, (i-1) + k)
        kmerstested <- c(kmerstested, posskmer)
        if(nchar(posskmer) == k)
          {
            match <- gregexpr(posskmer, window)
            
            if(match[[1]][1] == -1)
              nmatches <- 0
            else
              nmatches <- length(match[[1]])

            kmerstestednummatches <- c(kmerstestednummatches, nmatches)
            
            if (nmatches >= t)
              result <- c(result, posskmer)

            if(nmatches >= maxmatches)
              {
                maxmatches = nmatches
                #kmersmax <- c(kmersmax, posskmer)
              }
              
          } ## end of if(nchar(posskmer == k))

      } ## end of for(i in 1:L+k)

  } ## end of for(j in 1:nchar(string))

  #print(length(kmerstested))
  #print(kmerstested)
  
  ## match <- gregexpr(sequence, string)
  ## if(match[[1]][1] == -1)
  ##   nmatches <- 0
  ## else
  ##   nmatches <- length(match[[1]])
  

  ## print(paste("Number of matches found in the entire string = ", nmatches, sep=""))

  ## print(match[[1]])

  ## if(nmatches >= t)
  ##   {
  ##     if((match[[1]][t] + length(string) - match[[1]][1]) <= L)
  ##       result = TRUE
  ##   }

  ## if(nmatches == t)
  ##   {
  ##     result <- TRUE
  ##   }

  #return(result) ## Returns duplicated elements.  Perhaps return(unique(result)) instead?


  indices <- which(kmerstestednummatches == maxmatches)
  kmersmax <- kmerstested[indices]
  
  return(unique(kmersmax))
}
