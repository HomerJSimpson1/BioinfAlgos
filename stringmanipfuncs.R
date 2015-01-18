splitstring <- function(string, numchars) {
  ## Returns the string as a vector of substrings, all of length numchars.
  ## Klunky...
  starts <- seq(1,nchar(string), by=numchars)
  result <- sapply(starts, function(iii) { substr(string, iii, iii + (numchars - 1))})
  return(result)
}



splitstring2 <- function(string, numchars=3) {
  ## Same as splitstring(), but different methodology.
  #gregexpr and regmatches:

  ## Unfortunately, this first one drops any tail elements that form a group < numchars.
  ## i.e. if the string length is 21 and numchars = 6, then the last 3 characters are dropped.
  #result <- regmatches(string, gregexpr(paste(".{",numchars,"}",sep=""), string))[[1]] 
  ## OR can also use the below
  
  result <- strsplit(string, paste("(?<=.{",numchars,"})",sep=""), perl = TRUE)[[1]]
  return(result)
}





foo <- function(dfrow) {
  ## Paste together the column values in a row of a data frame.
  #stringres <- sapply(1:ncol(dfrow), paste0(dfrow, collapse=""))
  stringres <- paste0(dfrow, collapse="")
  return(stringres)
}



genallstrings <- function(alphabet, A, N) {
  ## Generate all possible strings of length N using the given alphabet of length A.
  ## e.g. alphabet in the bioinformatics realm will typically be c('A', 'C', 'G', 'T')
  vect <- vector('character')
  vects <- vector('character')
  
  for (i in 1:N) {
    for (j in 1:A) {      
      vec <- rep(alphabet[j], A^(N - i))      
      vect <- c(vect, vec)
    }
    vect <- rep(vect, A^(i-1))    
    vects <-cbind(vects, vect)
    vect <- vector('character')
  }

  df <- data.frame(vects, stringsAsFactors = FALSE)
  strng <- apply(df, 1, function(x) {foo(x)})
  indx <- seq(0, (nrow(df) - 1))
  
  df1 <- as.data.frame(cbind(indx,strng))
  return(df1)  
}



## An older version of the above function (genallstrings) that specifically worked
## with the alphabet c('A, 'C', 'G', 'T') and was not flexible enough to handle any
## given alphabet.
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







comparevec <- function(vec1, vec2) {
  ## Compare two vectors for equality
  ## Return the number of locations where the two vectors differ.
  result <- (vec1 == vec2)
  noteq <- result[result == FALSE]
  
  return(length(noteq))
}





readP <- function(input) {
  ## Convert input into a vector
  temp <- as.integer(strsplit(substr(input, 2, nchar(input) - 1), " ")[[1]])  
  return(temp)
}



printP <- function(P, fname="", tofile=TRUE, showsign=TRUE) {
  ## Print the P vectorlines to a file
  if(showsign) {
    nums <- paste(sprintf("%+d", P), "", collapse="")
    trimd <- substr(nums, 1, nchar(nums) - 1)
  }
  else {
    nums <- paste(sprintf("%d", P), "", collapse="")
    trimd <- substr(nums, 1, nchar(nums) - 1)
  }
  strOut <- paste("(", trimd, ")", sep="")
    
  if (tofile)
    write(strOut, fname, append=TRUE)
  else
    print(strOut)
}
