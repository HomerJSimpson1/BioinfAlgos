getallpatterns <- function(myString, patLength) {
  ## Get all patterns of length patLength in the string "mystring"
  strLength <- nchar(myString)
  patterns <- vector('character')
  
  for (i in 1:(strLength - patLength + 1)) {
    pattern <- substring(myString, i, (i + patLength -1))
    patterns <- c(patterns, pattern)
  }
  return(patterns)
}



getfeaspatterns <- function(theString, k, d) {
  ## Find all possible patterns from string of length k that are within hamming distance
  ## d of the patterns.
  df <- genperms(k)
  stringpats <- getallpatterns(theString, k)
  #df <- df$strng

  #mylist = lapply(stringpats, function(x) {gethamdlist(x, df$strng) })
  mylist = list()  
  for (i in 1:length(stringpats)) {
    pattern <- stringpats[i]
    patname <- paste(pattern, "", sep="")
    hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})    
    mylist[[i]] <- hamming[hamming <= d]
  }

  return(mylist)
}


gethamdlist <- function(mypattern, allperms) {
  #gethamdlist <- function(mystring, mypattern, mylist) {  
  patname <- paste(mypattern, "", sep="")
  hamming <- sapply(allperms, function(x) {findhamming1(x, mypattern)})    
  #mylist[[i]] <- hamming[hamming <= d]
  #mylist <- hamming[hamming <= d]  
}



findkmersmism <- function(aString, k, hamdist) {
  ## Find all kmers, allowing mismatches of up to Hamming Distance = d,
  ## within the given string.

  maxcount <- 0
  maxpatterns <- vector('character')

  time1 <- proc.time()
  
  thelist <- getfeaspatterns(aString, k, hamdist)

  #time2 <- proc.time()
  #print(time2 - time1)
  
  for (j in 1:length(thelist)) {
    ## Get the name of the current pattern from aString that is being tested.
    curpat <- names(thelist[[j]][thelist[[j]] == 0])
    counts <- sapply(names(thelist[[j]]), function(x) {findnummatches(aString, x)})
    count <- sum(counts)

    if (count == maxcount) {
      maxpatterns <- c(maxpatterns, curpat)
    }
    else if (count > maxcount) {
      maxcount <- count
      maxpatterns <- vector('character')
      maxpatterns <- c(maxpatterns, curpat)
    }
    
  } ## End for (j in 1:length(df)) loop

  time2 <- proc.time()
  print(time2 - time1)
  
    return(unique(maxpatterns))
}




findmaxpatterns <- function(string, curpattern, maxcount, maxpatterns) {
  #for (j in 1:length(patterns)) {
    #curpattern <- patterns[j]
  count <- findnummatches(string, curpattern)
  if (count == maxcount) {
    maxpatterns <- c(maxpatterns, curpattern)
  }
  else if(count > maxcount) {
    print(paste("New count is ", count, sep=""))
    maxcount = count
    maxpatterns <- vector('character')
    maxpatterns <- c(maxpatterns, curpattern)
  }
    
  #} ## End for (j in 1:length(patterns))

  return(maxpatterns)
  
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




countd <- function(string, pattern, d) {
  ## Find the number of patterns that are within Hamming Distance = d of pattern
  ## within the string.

  strLength <- nchar(string)
  patLength <- nchar(pattern)
  indices <- vector('numeric')
  count <- 0

  for(i in 1:(strLength - patLength + 1))
    {
    teststring <- substring(string, i, i + (patLength - 1))
    #print(teststring)
    hamdist <- findhamming(teststring, pattern)
    if (hamdist <= d)
      {
        ## Then this counts as a pattern. Increment count.
        count <- count + 1
        ## Use i-1 as the index because the class is counting
        ## the first position as 0, not 1.
        indices <- c(indices, (i-1))
      }
    
  } # End for loop

  retvals <- list(count, indices)
  #return(count)
  return(retvals)
  
}


charequal <- function(char1, char2) {
  ## Return TRUE if char1 == char2 and FALSE otherwise.
  if (char1 != char2)
    count <- 1
  else
    count <- 0
  #print(count)
  return(count)
}


findhamming1 <- function(string1, string2) {
  ## Find the Hamming Distance between the two input strings.
  ## If the two strings are not of equal length, then return an error message.
  ## Otherwise, find the Hamming Distance between the two strings.
  ## The Hamming Distance is defined as the minimum distance required to mutate one string into
  ## the other.
  
  ## If the strings are not of equal length, abort the function and return an error.
  if(nchar(string1) != nchar(string2))
    {
      print(string1)
      print(string2)
      
      stop("The two input strings must be of the same length.")
    }
  
  str1split <- strsplit(string1, NULL)[[1]]
  str2split <- strsplit(string2, NULL)[[1]]
  counts <- rep(0, length(str1split))
  df <- cbind(str1split, str2split, counts)
  counts <- mapply(function(x, y) {charequal(x, y)}, str1split, str2split)  

  return(sum(counts))
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
      print(string1)
      print(string2)
      
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
  df$strng <- apply(df, 1, function(x) {foo(x)})
  
  return(df)
}



applyfindhamming <- function(df, pattern, hammingdist) {
  ## Call Hamming Distance function to see how many of the possible permutations are within the
  ## specified Hamming Distance.
  df$hamming <- sapply(df$strng, function(x) {findhamming(x, pattern)})
  hamdist <- df[df$hamming <= hammingdist, ]
  return(hamdist)
}


