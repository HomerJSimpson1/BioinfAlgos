findclumps <- function(string, sequence, L, t) {
  ## Find the number of clumps of short sequence L in the long sequence "sequence".
  ## If the number of clumps == t, then we have a (L, t) clump

  result <- FALSE
  match <- gregexpr(sequence, string)
  if(match[[1]][1] == -1)
    nmatches <- 0
  else
    nmatches <- length(match[[1]])
  

  print(paste("Number of matches found in the entire string = ", nmatches, sep=""))

  print(match[[1]])

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
