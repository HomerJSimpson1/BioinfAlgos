findskew <- function(string) {
  ## Find and plot skew vs k, where k is the number of nucleotides thus far
  ## for which we have calculated skew (where skew = #G - #C
  ## findskew <- function(string, k) {
  
  numchars <- nchar(string)
  numG <- rep(0, numchars)
  Gs <- 0
  numC <- rep(0, numchars)
  Cs <- 0
  #GminusC <- rep(0, numchars)

  k <- 0:(numchars - 1)
  vectString <- strsplit(string,"")[[1]]

  for(i in 1:numchars) {
    curChar <- vectString[i]

    if(curChar == 'G')
        Gs <- Gs + 1
    else if(curChar == 'C')
        Cs <- Cs + 1

    numG[i] <- Gs
    numC[i] <- Cs    
  }

  
  df <- data.frame(vectString, k, numG, numC, stringsAsFactors = FALSE)
  df$GminusC <- df$numG - df$numC

  return(df)
}
