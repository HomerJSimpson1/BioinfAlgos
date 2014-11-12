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

  ## Else calculate the Hamming Distance
  #counts <- rep(0, nchar(string1))
  #strings <- cbind(string1, string2, counts)
  
  #result <- apply(strings, 1, function(x) { if(x[1] != x[2]) strings[3] <- 1})

  count = 0

  #print(nchar(string1))

  for(i in 1:nchar(string1))
    {
      #print(i)
      #print(substring(string1, i, i))
      #print(substring(string2, i, i))

      if(substring(string1, i, i) != substring(string2, i, i))
        {
          count <- count + 1
        }
    }

  return(count)

  #return(as.data.frame(cbind(strings, result)))
  #return(strings)
  #return(sum(strings[3]))
}
