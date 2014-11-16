
patterntonumber <- function(df, pattern) {
  ## Return the index of pattern
  return(df[df$strng == pattern,]$indx)
}

numbertopattern <- function(df, indx) {
  ## Return the pattern at index "indx"
  return(df$strng[indx])
}



foo <- function(dfrow) {
  ## Paste together the column values in a row of a data frame.
  #stringres <- sapply(1:ncol(dfrow), paste0(dfrow, collapse=""))
  stringres <- paste0(dfrow, collapse="")
  return(stringres)
}



getallpatterns <- function(myString, patLength) {
  ## Get all patterns of length patLength in the string "mystring"
  strLength <- nchar(myString)
  #patterns <- data.frame(indx=as.numeric(), pattern=as.character(), frequency=as.numeric(), stringAsFactors=FALSE)
  #print(patterns)
  patterns=as.character()
  indices=as.numeric()
  frequency=as.numeric()
  maxindx <- 1
  
  for (i in 1:(strLength - patLength + 1)) {
    pattern <- substring(myString, i, (i + patLength -1))
    ## If pattern is in patterns, increment the frequency variable.
    ## Otherwise, add it to patterns and set its frequency variable
    ## to a value = 1.
    #if (pattern %in% patterns[,1])
    #matchindx <- match(pattern, patterns[,2], -1)
    matchindx <- match(pattern, patterns, -1)
    #print(matchindx)
    if (matchindx > 0)
      {
        #patterns[matchindx,3] <- patterns[matchindx,3] + 1
        frequency[matchindx] <- frequency[matchindx] + 1
      }
    else
      {
        #print(maxindx)
        #print(pattern)
        #patternadd <- cbind(as.numeric(maxindx), as.character(pattern), as.numeric(1))
        #print(patternadd)
        #patterns <- rbind(patterns, patternadd)
        ## patterns[maxindx,1] <- as.numeric(maxindx - 1)
##         patterns[maxindx,2] <- as.character(pattern)
##         patterns[maxindx,3] <- as.numeric(1)

        indices <- c(indices, maxindx - 1)
        patterns <- c(patterns, pattern)
        frequency <- c(frequency, 1)
        maxindx <- maxindx + 1
      }
  } ## End for (i in 1:(strLength - patLength + 1))
  options(stringsAsFactors=FALSE)
  ## patternset <- data.frame(cbind(indx=as.numeric(indices),
##                                  pattern=as.character(patterns, stringsAsFactors=FALSE),
##                                  frequency=as.numeric(frequency)))
  patternset <- data.frame(cbind(indx=as.numeric(indices),
                                 pattern=as.character(patterns),
                                 frequency=as.numeric(frequency)))  
  patternset$indx = as.numeric(patternset$indx)
  patternset$frequency = as.numeric(patternset$frequency)
  #return(patterns)
  return(patternset)
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
