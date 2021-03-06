neighbors1 <- function(pattern, d) {
  ## Coursera Bioinformatic Algorithms Part 1 Class
  ## Implements the "Neighbors" function provided in the
  ## interactive text.  See "Charging Station: Generating the
  ## Neighborhood of a String"

  alphabet = c('A', 'C', 'G', 'T')
  if (d == 0)
    {
      print("d = 0")
      return(pattern)
    }
  if (nchar(pattern) == 1)
      return(alphabet)
  neighborhood <- vector('character')
  suffixneighbors <- vector('character')
  suffixneighbors <- c(neighbors1(suffix(pattern), d), suffixneighbors)  

  #print(suffixneighbors)
  
  for (i in 1:length(suffixneighbors)) {
    text <- suffixneighbors[i]
    sufx <- suffix(pattern)
    ham <- findhamming(sufx, text)
    #print(ham)
    #if (findhamming(sufx, text) < d)
    if (ham < d)      
      {
        #print(paste("Value of text is currently:", text, sep=" "))
        #print(paste("Hamming distance of ", sufx, "is", ham, sep=" "))
        for (j in 1:length(alphabet)) {
          ##print(alphabet[j])
          ##nextadd <- paste(alphabet[j], text, sep="")
          ##if (length(nextadd == length(pattern)
          ##if (findhamming(nextadd, pattern) == d)
          ##  neighborhood <- c(paste(alphabet[j], text, sep=""), neighborhood)
          neighborhood <- c(paste(alphabet[j], text, sep=""), neighborhood)
        } ## end for (j in 1:nchar(sufx))
        
      } ## end if (findhamming())
    else
      {
        #print(paste("Neighborhood is currently: ", neighborhood, sep=""))
        firstsym <- firstsymbol(pattern)
        nextadd <- paste(firstsym, text, sep="")
        print(paste("Next AA to add=", nextadd, "and the hamming distance from pattern", pattern, "is", findhamming(nextadd, pattern), sep=" "))
        if (findhamming(nextadd, pattern) == d)
          neighborhood <- c(nextadd, neighborhood)
        ## neighborhood <- c(paste(firstsym, text, sep=""), neighborhood)
      } ## end else
   
  }## end for (i in 1:length())

  return(neighborhood)
  
} ## end neighbors1 function






neighbors <- function(pattern, d) {
  ## Coursera Bioinformatic Algorithms Part 1 Class
  ## Implements the "Neighbors" function provided in the
  ## interactive text.  See "Charging Station: Generating the
  ## Neighborhood of a String"

  alphabet = c('A', 'C', 'G', 'T')
  if (d == 0)
    return(pattern)
  if (nchar(pattern) == 1)
      return(alphabet)
  neighborhood <- vector('character')
  suffixneighbors <- vector('character')
  suffixneighbors <- c(neighbors(suffix(pattern), d), suffixneighbors)  

  for (i in 1:length(suffixneighbors)) {
    text <- suffixneighbors[i]
    sufx <- suffix(pattern)
    if (findhamming(sufx, text) < d)
      {
        for (j in 1:length(alphabet)) {
          neighborhood <- c(paste(alphabet[j], text, sep=""), neighborhood)                    
        } ## end for (j in 1:nchar(sufx))
        
      } ## end if (findhamming())
    else
      {
        firstsym <- firstsymbol(pattern)
        neighborhood <- c(paste(firstsym, text, sep=""), neighborhood)        
      } ## end else
    
  }## end for (i in 1:length())

  return(neighborhood)
  
} ## end neighbors function





firstsymbol <- function(pattern) {
  ## Helper function
  ## Returns the first symbol of pattern
  patvec <- strsplit(pattern, "")[[1]]
  #firstsymb <- patvec[1]
  return(patvec[1])
}

suffix <- function(pattern) {
  ## Helper function
  ## Returns the 'suffix' of a pattern, i.e. all of the pattern
  ## except for the first element of the pattern.

  patvec <- strsplit(pattern, "")[[1]]
  #print(patvec)
  #print(patvec[2:length(patvec)])
  patvec <- patvec[2:length(patvec)]
  return(paste(patvec, "", sep="", collapse=""))
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
