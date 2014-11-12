findclumps2 <- function(string, pattern) {
  ## Find the number of occurences of pattern in string.
  ## This version works correctly.

  patLength <- nchar(pattern)
  stringLength <- nchar(string)

  count <- 0
  
  for(i in 1:(stringLength - patLength + 1))
    {
      #print(substring(string, i, i + (patLength - 1)))
      if(substring(string, i, i + (patLength - 1)) == pattern)
      {
        count <- count + 1
       }
    }

  return(count)
}
