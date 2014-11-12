findkmers2 <- function(string, k, t) {
  ## Find the number of occurences of pattern in string.
  ## This version works correctly.

  #patLength <- nchar(pattern) ## This is k in this function
  stringLength <- nchar(string)

  count <- 0
  #indices <- vector('numeric')
  patterns <- vector('character')
  results <- cbind(vector('character'), vector('numeric'))
  
  for(i in 1:(stringLength - k + 1))
    {
      #print(substring(string, i, i + (patLength - 1)))
      #if(substring(string, i, i + (patLength - 1)) == pattern)
      pattern = substring(string, i, i + k)
      #patterns <- c(patterns, pattern)
      indices <- vector('numeric')
      indices <- c(indices, i)
      for(j in (i+1):(stringLength - k)) {
        if(substring(string, j, j + k) == pattern)
          {
            indices <- c(indices, j)
            #count <- count + 1
          }
      } ## End for (j ....)

      if(length(indices) > t)
         {
           result <- cbind(pattern, indices)
           print(result)
           
           results <- rbind(results, result)
           #patterns <- c(patterns, pattern)
           #indices <- c(indices, indices
         }
    } ## End for (i ...)

  #return(count)
  return(results)

}
