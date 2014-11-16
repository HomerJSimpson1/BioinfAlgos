
patterntonumber <- function(df, pattern) {
  ## Return the index of pattern
  return(df[df$strng == pattern,]$indx)
}

numbertopattern <- function(df, indx) {
  ## Return the pattern at index "indx"
  return(df$strng[indx + 1])
}





foo <- function(dfrow) {
  ## Paste together the column values in a row of a data frame.
  #stringres <- sapply(1:ncol(dfrow), paste0(dfrow, collapse=""))
  stringres <- paste0(dfrow, collapse="")
  return(stringres)
}




genperms <- function(k) {
  ## Generate all permutations of length k for a given alphabet.

  ## Assuming an alphabet here of {"A", "C", "G", "T"}
  alphabet = c('A', 'C', 'G', 'T')

  #patLength <- nchar(pattern)
  vects <- vector('character')

  for (i in 1:k) {
    vectA <- rep("A", 4^(k - i))
    vectC <- rep("C", 4^(k - i))
    vectG <- rep("G", 4^(k - i))
    vectT <- rep("T", 4^(k - i))
    vect <- c(vectA, vectC, vectG, vectT)
    vects <-cbind(vects, vect)
  }

  ## for (i in 1:patLength) {
  ##   vectA <- rep("A", 4^(patLength - i))
  ##   vectC <- rep("C", 4^(patLength - i))
  ##   vectG <- rep("G", 4^(patLength - i))
  ##   vectT <- rep("T", 4^(patLength - i))
  ##   vect <- c(vectA, vectC, vectG, vectT)
  ##   vects <-cbind(vects, vect)
  ## }  

  df <- data.frame(vects, stringsAsFactors = FALSE)
  strng <- apply(df, 1, function(x) {foo(x)})
  indx <- seq(0, (nrow(df) - 1))
  
  df1 <- as.data.frame(cbind(indx,strng))
  return(df1)
}






findnummatches <- function(string, sequence) {
  match <- gregexpr(paste("(?=", sequence, ")", sep=""), string, perl=TRUE)
  #print(match[[1]])
  if(match[[1]][1] == -1)
    nmatches <- 0
  else
    nmatches <- length(match[[1]])
  return(nmatches)
}




kmerfrequency <- function(allposskmers, searchstring) {
  ## Given the set of all possible kmers (of length 4^k)
  ## and the string in which to find the kmers, return the
  ## frequency of each kmer found in the searchstring.
  dflength <- nrow(allposskmers)
  counts <- as.numeric()
  
  for (i in 1:dflength) {
    ## Iterate through the data frame of kmers
    pattern <- allposskmers$strng[i]
    count <- findnummatches(searchstring, pattern)
    counts <- c(counts, count)
  }

  allposskmers <- cbind(allposskmers, counts)
  return(allposskmers)
}





frequentwords <- function(string, k, d) {
  ## Generate a frequency array that includes all possible kmers
  ## of length k that can be found within the string to be searched,
  ## given by the "string" input parameter. d = Hamming Distance.

  kperms <- genperms(k)
  print(head(kperms))
  freqarray <- kmerfrequency(kperms, string)

  return(freqarray)

}









## getallpatterns <- function(myString, patLength) {
##   ## Get all patterns of length patLength in the string "mystring"
##   strLength <- nchar(myString)
  
##   ## Initialize vectors and variables
##   patterns=as.character()
##   indices=as.numeric()
##   frequency=as.numeric()
##   maxindx <- 1

##   ## Step through the string and record all existing patterns and
##   ## their observed frequency.  Includes overlapping patterns.
##   for (i in 1:(strLength - patLength + 1)) {
##     pattern <- substring(myString, i, (i + patLength -1))
##     ## If pattern is in patterns, increment the frequency variable.
##     ## Otherwise, add it to patterns and set its frequency variable
##     ## to a value = 1.
##     matchindx <- match(pattern, patterns, -1)
##     if (matchindx > 0)
##       {
##         frequency[matchindx] <- frequency[matchindx] + 1
##       }
##     else
##       {
##         indices <- c(indices, maxindx - 1)
##         patterns <- c(patterns, pattern)
##         frequency <- c(frequency, 1)
##         maxindx <- maxindx + 1
##       }
##   } ## End for (i in 1:(strLength - patLength + 1))
##   options(stringsAsFactors=FALSE)
##   patternset <- data.frame(cbind(indx=as.numeric(indices),
##                                  pattern=as.character(patterns),
##                                  frequency=as.numeric(frequency)))  
##   patternset$indx = as.numeric(patternset$indx)
##   patternset$frequency = as.numeric(patternset$frequency)

##   return(patternset)
## }




