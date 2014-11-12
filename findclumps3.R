findclumps3 <- function(string, pattern) {
  ## Find the indices of all occurences of pattern in string.
  ## This version works correctly.

  patLength <- nchar(pattern)
  stringLength <- nchar(string)

  #count <- 0
  indices <- vector('numeric')
  
  for(i in 1:(stringLength - patLength + 1))
    {
      #print(substring(string, i, i + (patLength - 1)))
      if(substring(string, i, i + (patLength - 1)) == pattern)
      {
        #count <- count + 1
        #indices <- c(indices, i)
        ## Return index - 1 for each of the indices, because the BioInformatics
        ## Algorithms class is expecting zero-based indexing and R uses one-based
        ## not zero-based.
        indices <- c(indices, i - 1)
       }
    }
  print(indices)
  #return(count)
  return(indices)
}
