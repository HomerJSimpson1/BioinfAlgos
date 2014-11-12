countd <- function(string, pattern, d) {
  ## Find the number of patterns that are within Hamming Distance = d of pattern
  ## within the string.

  strLength <- nchar(string)
  patLength <- nchar(pattern)
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
      }
    
  } # End for loop

  return(count)
  
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
