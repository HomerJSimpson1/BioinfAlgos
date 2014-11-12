findclumpsalt <- function(string, sequence, L, t) {
  ## Find the number of clumps of short sequence L in the long sequence "sequence".
  ## If the number of clumps == t, then we have a (L, t) clump

  result <- FALSE
  #match <- gregexpr(sequence, string)
  match <- gregexpr(paste("(?=", sequence, ")", sep=""), string, perl=TRUE)
  if(match[[1]][1] == -1)
    nmatches <- 0
  else
    nmatches <- length(match[[1]])
  

  #print(paste("Number of matches found in the entire string = ", nmatches, sep=""))

  #print(match[[1]])

  if(nmatches >= t)
    {
      if((match[[1]][t] + length(string) - match[[1]][1]) <= L)
        result = TRUE
    }

  ## if(nmatches == t)
  ##   {
  ##     result <- TRUE
  ##   }

  return(result)
}



walkstring <- function(string, k, L, t) {
  ## Walk the string and check for patterns
  strLength <- nchar(string)
  print(strLength)
  testedpatterns <- vector('character')
  goodpattern <- vector('character')
  
  for(i in 1:(strLength - k + 1)) {
    pattern <- substring(string, i, i + k - 1)
    if (!(pattern %in% testedpatterns)) {
      testedpatterns <- c(testedpatterns, pattern)
      kmerclump <- findclumpsalt(string, pattern, L, t)
      if(kmerclump)
        goodpattern <- c(goodpattern, pattern)
    }

  }
  print(goodpattern)
  return(goodpattern)
}
