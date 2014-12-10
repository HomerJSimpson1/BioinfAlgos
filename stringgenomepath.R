stringgenomepath <- function(patterns) {
  ## Input:  A sequence of k-mers Pattern1, ¡­ ,Patternn such that the last k - 1
  ##         symbols of Patterni are equal to the first k-1 symbols of Patterni+1
  ##         for 1 ¡Ü i ¡Ü n-1.
  ## Output: A string Text of length k+n-1 such that the i-th k-mer in Text is equal
  ##         to Patterni  (for 1 ¡Ü i ¡Ü n)

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


lastsymbol <- function(pattern) {
  ## Return the last symbol of pattern
  return(substr(pattern, nchar(pattern), nchar(pattern)))
}



getkminus1 <- function(pattern, firstorlast="first") {
  ## getkminus1 <- function(pattern, k, firstorlast="first") {  
  ## Get either the first k-1 symbols or the last k-1 symbols from pattern and return.

  k <- nchar(pattern)
  
  if(firstorlast == "first")
    {
      #print(firstorlast)
      result <- substr(pattern, 1, k - 1)
    }
  else if(firstorlast == "last")
    {
      print(firstorlast)
      #print(nchar(pattern))
      #print(nchar(pattern) - (k - 1))
      #result <- substr(pattern, nchar(pattern) - (k - 1) + 1, k)
      #result <- substr(pattern, k - (k - 1) + 1, k)
      result <- substr(pattern, 2, k)
    }
  else
    {
      stop("That is not a valid choice for first or last. Please choose exactly one of 'first' or 'last'.")
    }

  return(result)
}
