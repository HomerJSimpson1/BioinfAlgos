probkmerinstring <- function(N, A, pattern, t) {
  ## Calculate the probability of a pattern, using an alphabet of length A, occurs
  ## at least t times in a string of length N.

  ## This uses a brute force solution.

  count <- 0
  patlength <- nchar(pattern)
  alphabet <- c(0,1)
  
  Nstrings <- genallstrings(alphabet, A, N)

  ## for (i in 1:nrow(Nstrings)) {
  ##   if (pattern %in% Nstrings$strng[i]) {
  ##     count <- count + 1
  ##     print(count)
  ##   }
  ## }
  
  for (i in 1:nrow(Nstrings)) {
    nummatch <- findnummatches(Nstrings$strng[i], pattern)
    if (nummatch >= t) {
      count <- count + 1
    }
  }
  
  return(count / nrow(Nstrings))
}



## probkmersmarter <- function(N, A, pattern, t) {
##   ## Smarter way to calculate the probability in probkmerinstring
##   ## Does not use a brute force solution
##   ## But I don't know how to generalize this to any pattern and any value of t.
##   ## For the pattern = "01" and t=1, it's (A^N - (N+1)) / A^N
##   ## Pr(25, 2, 01, 1) = (2^25 - 26) / 2^25
##   ## Pr(4, 2, 01, 1) = (2^4 - 5) / 2^4
##   ## BUT:
##   ## Pr(4, 2, 11, 1) = (2^4 - 8) / 2^4
##   return((A^N -  /A^N)
## }


probapprox <- function(N, A, k, t) {
  ## Approximates the probability of a kmer, using an alphabet of length A, will
  ## appear t or more times in a string of length N.  Does better when there are
  ## few overlaps.  See the interactive text.
  ## Pr(N, A, k, t) approximately = (1 - (1 - p)^A^k) where p = ((N - t*(k-1)) choose t) / A^(t*k) OR
  ## Pr(N, A, k, t) approximately = ((N - t(k-1)) choose  t) / (A^(t*k) * A^k) OR
  ## Pr(N, A, k, t) approximately = ((N - t(k-1)) choose  t) / (A^((t*k) + k)) OR
  ## Pr(N, A, k, t) approximately = ((N - t(k-1)) choose  t) / A^((t-1) * k)
  ## where the choose function is: choose(n, k) = n! / (k! * (n-k)!)

  ##p <- choose((N - t * (k - 1)), t) / A^(t * k)
  ##temp <- 1 - (1 - p)^(A^k)
  ##print(p)
  
  numerator <- choose((N - (t * (k - 1))), t)
  #denominator <- A^((t - 1) * k)
  denominator <- A^(t * k + k)  

  #print(numerator)
  #print(denominator)
  return(numerator / denominator)
}




foo <- function(dfrow) {
  ## Paste together the column values in a row of a data frame.
  #stringres <- sapply(1:ncol(dfrow), paste0(dfrow, collapse=""))
  stringres <- paste0(dfrow, collapse="")
  return(stringres)
}


genallstrings <- function(alphabet, A, N) {
  ## Generate all possible strings of length N using the given alphabet of length A.
  vect <- vector('character')
  vects <- vector('character')
  
  for (i in 1:N) {
    #print(i)
    #for (j in 1:length(alphabet)) {
    for (j in 1:A) {      
      #vec <- rep(alphabet[j], 4^(N - i))
      vec <- rep(alphabet[j], A^(N - i))      
      #print(vec)
      vect <- c(vect, vec)
      #print(vect)
      #print(4^(i-1))

      ## vect <- cbind(vect, vec)
      ## paste("vec",j,sep="") <- rep(alphabet[j], 4^(N - i))
      ## vect <- c(vect, paste("vec", j, sep=""))
      ## print(vect)
      ## vects <-cbind(vects, vect)      
    }
    ## vectA <- rep("A", 4^(k - i))
    ## vectC <- rep("C", 4^(k - i))
    ## vectG <- rep("G", 4^(k - i))
    ## vectT <- rep("T", 4^(k - i))
    ## vect <- c(vectA, vectC, vectG, vectT)
    #vect <- rep(vect, 4^(i-1))
    vect <- rep(vect, A^(i-1))    
    #print(vect)
    
    vects <-cbind(vects, vect)
    vect <- vector('character')
  }

  df <- data.frame(vects, stringsAsFactors = FALSE)
  strng <- apply(df, 1, function(x) {foo(x)})
  indx <- seq(0, (nrow(df) - 1))
  
  df1 <- as.data.frame(cbind(indx,strng))
  return(df1)  
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
