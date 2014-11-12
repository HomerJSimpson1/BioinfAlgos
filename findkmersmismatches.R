findkmersmismatches <- function(string, k, d) {
  ## Find all kmers, allowing mismatches of up to Hamming Distance = d,
  ## within the given string.

  strLength <- nchar(string)
  maxcount <- 0
  maxpatterns <- vector('character')

  for(i in 1:(strLength - k + 1)) {
    pattern <- substring(string, i, (i + k - 1))
    print(pattern)
    #patterns <- permutestring2(pattern, d)$strng
    patterns <- permutestring3(pattern, d)$strng    
    for (j in 1:length(patterns)) {
      curpattern <- patterns[j]
      count <- countd(string, curpattern, d)[[1]]
      if (count == maxcount) {
        maxpatterns <- c(maxpatterns, curpattern)
      }
      else if(count > maxcount) {
        print(paste("New count is ", count, sep=""))
        maxcount = count
        maxpatterns <- vector('character')
        maxpatterns <- c(maxpatterns, curpattern)
        print(maxpatterns)
      }
      
    } ## End for (j in 1:length(patterns))
    
  } ## End for (i in 1:(strLength - k + 1))

  return(unique(maxpatterns))
}


countd <- function(string, pattern, d) {
  ## Find the number of patterns that are within Hamming Distance = d of pattern
  ## within the string.

  strLength <- nchar(string)
  patLength <- nchar(pattern)
  indices <- vector('numeric')
  count <- 0

  for(i in 1:(strLength - patLength + 1))
    {
    teststring <- substring(string, i, i + (patLength - 1))
    print(teststring)
    hamdist <- findhamming(teststring, pattern)
    if (hamdist <= d)
      {
        ## Then this counts as a pattern. Increment count.
        count <- count + 1
        ## Use i-1 as the index because the class is counting
        ## the first position as 0, not 1.
        indices <- c(indices, (i-1))
      }
    
  } # End for loop

  retvals <- list(count, indices)
  #return(count)
  return(retvals)
  
}



findhamming <- function(string1, string2) {
  ## Find the Hamming Distance between the two input strings.
  ## If the two strings are not of equal length, then return an error message.
  ## Otherwise, find the Hamming Distance between the two strings.
  ## The Hamming Distance is defined as the minimum distance required to mutate one string into
  ## the other.

  ## If the strings are not of equal length, abort the function and return an error.
  if(nchar(string1) != nchar(string2))
    {
      print(string1)
      print(string2)
      
      stop("The two input strings must be of the same length.")
    }

  count = 0

  for(i in 1:nchar(string1))
    {
      if(substring(string1, i, i) != substring(string2, i, i))
        {
          count <- count + 1
        }
    }

  return(count)
}
