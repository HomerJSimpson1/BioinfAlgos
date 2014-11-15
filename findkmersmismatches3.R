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
  #print(stringpats)

  #print(head(df))
  #df <- as.data.frame(df$strng)
  df <- df$strng

  #return(df)
  #colnames(df) <- c('col1', 'col2', 'col3', 'col4', 'strng')
  #namevec <- c('col1', 'col2', 'col3', 'col4', 'strng)'
  #namevec <- c('strng')
  #print(df$names)

  #print(length(stringpats))

  mylist = list()
  #df$hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})
  for (i in 1:length(stringpats)) {
    pattern <- stringpats[i]
    patname <- paste(pattern, "", sep="")
    #print(patname)
    ## df$paste(pattern) <- pattern
    ## print(head(df))
    #hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})
    hamming <- sapply(df, function(x) {findhamming1(x, pattern)})

    #print(str(hamming))
    #print(rownames(hamming))
    print(names(hamming))
    #print(length(hamming))
    #print(length(df))
    df1 <- as.data.frame(cbind(df, hamming))
    #df1 <- df1[df1$hamming <= d,]
    mylist[[i]] <- df1
    names(mylist[[i]]) <- patname
    #str(hamming)
    #print(names(hamming)) # These are row names
    #print(min(hamming))
    ## if (min(hamming) <= d) {
    ##   df <- cbind(df, hamming)
    ##   namevec <- c(namevec, patname)
    ## }
    #colnames(df) <- c(colnames(df), patname)

    #print(df$names)
    ## print(head(hamming))
    ## mylist[i] <- c(pattern, df$strng, hamming)
    #print(head(mylist[i]))
  }

  #colnames(df) <- namevec
  #print(d)

  #dfhamd <- df[df$hamming <= d, ]
  #for (i in 1:length(stringpats)) {
  #  dfhamd <- df[df <= d,]
  #}
  
  #print(head(dfhamd))
  #return(df)
  return(mylist)
  #return(dfhamd)
  #return(dfhamd$strng)
}




findkmersmism <- function(aString, k, hamdist) {
  ## Find all kmers, allowing mismatches of up to Hamming Distance = d,
  ## within the given string.

  maxcount <- 0
  maxpatterns <- vector('character')

  print(hamdist)

  df <- getfeaspatterns(aString, k, hamdist)
  pats <- df$strng
  for (j in 1:length(pats)) {
    #count <- countd(aString, df[j], hamdist)[[1]]
    count <- findnummatches(aString, pats[j])
    if (count == maxcount) {
      #maxcount = count
      maxpatterns <- c(maxpatterns, pats[j])
    }
    else if (count > maxcount) {
      maxcount <- count
      maxpatterns <- vector('character')
      maxpatterns <- c(maxpatterns, pats[j])
    }
    
  } ## End for (j in 1:length(df)) loop

    return(unique(maxpatterns))
}





## findkmersmismatches <- function(string, k, d) {


##   strLength <- nchar(string)
##   maxcount <- 0
##   maxpatterns <- vector('character')

##   df <- genperms(k)

##   for(i in 1:(strLength - k + 1)) {
##     pattern <- substring(string, i, (i + k - 1))

##     ## Call Hamming Distance function to see how many of the possible permutations are within the
##     ## specified Hamming Distance.
##     df$hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})
##     patterns <- df[df$hamming <= d, ]$strng

##     maxpatterns <- sapply(patterns, function(w, x, y, z) findmaxpatterns(w, x, y, z), w=string,
##                           y = maxcount, z = maxpatterns)
    
##   } ## End for (i in 1:(strLength - k + 1))
    
##   return(unique(maxpatterns))
## }






## findkmersmismatches <- function(string, k, d) {
##   ## Find all kmers, allowing mismatches of up to Hamming Distance = d,
##   ## within the given string.

##   strLength <- nchar(string)
##   maxcount <- 0
##   maxpatterns <- vector('character')

##   df <- genperms(k)

##   for(i in 1:(strLength - k + 1)) {
##     pattern <- substring(string, i, (i + k - 1))

##     ## Call Hamming Distance function to see how many of the possible permutations are within the
##     ## specified Hamming Distance.
##     df$hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})    
##     patterns <- df[df$hamming <= d, ]$strng

##     maxpatterns <- sapply(patterns, function(w, x, y, z) findmaxpatterns(w, x, y, z), w=string,
##                           y = maxcount, z = maxpatterns)    

##     #print(head(patterns))
    
##     ## for (j in 1:length(patterns)) {
##     ##   curpattern <- patterns[j]
##     ##   count <- countd(string, curpattern, d)[[1]]
##     ##   if (count == maxcount) {
##     ##     maxpatterns <- c(maxpatterns, curpattern)
##     ##   }
##     ##   else if(count > maxcount) {
##     ##     #print(paste("New count is ", count, sep=""))
##     ##     maxcount = count
##     ##     maxpatterns <- vector('character')
##     ##     maxpatterns <- c(maxpatterns, curpattern)
##     ##     #print(maxpatterns)
##     ##   }
      
##     ## } ## End for (j in 1:length(patterns))
    
##   } ## End for (i in 1:(strLength - k + 1))

##   ## maxp <- unique(maxpatterns)
##   ## for (i in 1:length(maxp)) {
##   ##   for(j in (i + 1):length(maxp)){
##   ##     dist <- findhamming1(maxp[i], maxp[j])
##   ##     if (dist <= d)
        
##   ##   }
##   ## }
##   ## return(maxp)  
##   return(unique(maxpatterns))

## }




findkmersmismatches <- function(string, k, d) {
  ## Find all kmers, allowing mismatches of up to Hamming Distance = d,
  ## within the given string.

  ## Initialize some variables
  strLength <- nchar(string)
  maxcount <- 0
  maxpatterns <- vector('character')

  ## Generate all possible permutations of length k.
  df <- genperms(k)

  ## Iterate through all possible patterns in the string.  Starting at position 1,
  ## take the first substring of length k, and find how many patterns exist within Hamming
  ## Distance d of that pattern.  Then find the number of times any of those patterns occurs
  ## within the string, and sum them.  This yields the total number of times the first pattern
  ## appears within the given string, counting mismatches within Hamming Distance d.
  ## Now grab the next pattern in the string, starting at position 2.  Repeat the above steps
  ## for each substring of length k from the original string.  Then grab the next pattern in
  ## the string of length k, starting at position 3. Repeat, repeat, repeat... until there are
  ## no more substring patterns to examine in the string.
  for(i in 1:(strLength - k + 1)) {
    pattern <- substring(string, i, (i + k - 1))
    ## Call Hamming Distance function to see how many of the possible permutations are within the
    ## specified Hamming Distance.
    df$hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})    
    patterns <- df[df$hamming <= d, ]$strng

    print(head(patterns))

    #maxpatterns <- findmaxpatterns(string, patterns[1], maxcount, maxpatterns)

    #maxpatterns <- sapply(patterns, function(w, x, y, z) findmaxpatterns(w, x, y, z), w=string,
    #                      y = maxcount, z = maxpatterns)    

    for (j in 1:length(patterns)) {
      curpattern <- patterns[j]
      #maxpatterns[i] = findmaxpatterns(string, curpattern, maxcount, maxpatterns)
      #maxpatterns <- findmaxpatterns(string, curpattern, maxcount, maxpatterns)      
      count <- countd(string, curpattern, d)[[1]]
      #count <- findnummatches(string, curpattern)
      if (count == maxcount) {
        maxpatterns <- c(maxpatterns, curpattern)
      }
      else if(count > maxcount) {
        maxcount = count
        maxpatterns <- vector('character')
        maxpatterns <- c(maxpatterns, curpattern)
      }
      
    } ## End for (j in 1:length(patterns))
    
  } ## End for (i in 1:(strLength - k + 1))

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









## findkmersmismatches <- function(string, k, d) {
##   ## Find all kmers, allowing mismatches of up to Hamming Distance = d,
##   ## within the given string.

##   strLength <- nchar(string)
##   maxcount <- 0
##   maxpatterns <- vector('character')

##   df <- genperms(k)

##   for(i in 1:(strLength - k + 1)) {
##     pattern <- substring(string, i, (i + k - 1))
##     #print(pattern)
##     #patterns <- permutestring2(pattern, d)$strng
##     #patterns <- permutestring3(pattern, d)$strng
##     #patterns <- permutestring4(df, pattern, d)$strng
##     #patterns <- applyfindhamming(df, pattern, d)$strng
##     ## Call Hamming Distance function to see how many of the possible permutations are within the
##     ## specified Hamming Distance.
##     #df$hamming <- sapply(df$strng, function(x) {findhamming(x, pattern)})
##     df$hamming <- sapply(df$strng, function(x) {findhamming1(x, pattern)})    
##     patterns <- df[df$hamming <= d, ]$strng

##     #print(head(patterns))
    
##     for (j in 1:length(patterns)) {
##       curpattern <- patterns[j]
##       count <- countd(string, curpattern, d)[[1]]
##       if (count == maxcount) {
##         maxpatterns <- c(maxpatterns, curpattern)
##       }
##       else if(count > maxcount) {
##         #print(paste("New count is ", count, sep=""))
##         maxcount = count
##         maxpatterns <- vector('character')
##         maxpatterns <- c(maxpatterns, curpattern)
##         #print(maxpatterns)
##       }
      
##     } ## End for (j in 1:length(patterns))
    
##   } ## End for (i in 1:(strLength - k + 1))

##   return(unique(maxpatterns))
## }

