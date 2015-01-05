lcsback <- function(v, w) {
  ## Input: 2 strings
  ## Output: The path through the graph creating the
  ## longest common subsequence.

  vlen <- nchar(v)
  wlen <- nchar(w)  
  BIGINT <- 10000000

  vvec <- strsplit(v, "")[[1]]
  wvec <- strsplit(w, "")[[1]]

  s <- matrix(-BIGINT, nrow = (vlen + 1), ncol = (wlen + 1))
  backtrack <- matrix(, nrow = (vlen + 1), ncol = (wlen + 1))

  for (i in 1:(vlen + 1)) {
    s[i, 1] <- 0
  }

  for (j in 1:(wlen + 1)) {
    s[1, j] <- 0
  }

  for (i in 2:(vlen + 1)) {
    for (j in 2:(wlen + 1)) {
      if (vvec[i-1] == wvec[j-1]){        
        s[i, j] <- max(s[i-1, j], s[i, j-1], s[i-1, j-1] + 1)
      }
      else {
        s[i, j] <- max(s[i-1, j], s[i, j-1])
      } ## end else

      if (s[i, j] == s[(i-1), j])
        backtrack[i, j] <- 'd'
      else if (s[i, j] == s[i, (j-1)])
        backtrack[i, j] <- 'r'
      else if (s[i, j] == (s[(i-1), (j-1)] + 1)) {
        backtrack[i, j] <- 'g'
      }
        
    } ## end for (j in 2:(wlen + 1))
  } ## end for (i in 2:(vlen + 1))

  return(backtrack)
}





outputlcs <- function(backtrack, v, i, j, outpt) {
  ## Input: backtrack matrix, character vector v,
  ## i, and j
  ## Output: the longest common subsequence (LCS)

  while (i > 1 && j > 1) {
    if (backtrack[i, j] == 'd') {
      i = i - 1
    }
    else if (backtrack[i, j] == 'r') {
      j = j - 1
    }
    else if (backtrack[i, j] == 'g') {
      i = i - 1
      j = j - 1
      outpt <- c(outpt, v[i])
    }

  } ## end while loop

  return(outpt)
}
  


calllcsback <- function(v, w) {
  vlen <- nchar(v)
  wlen <- nchar(w)
  vvec <- strsplit(v, "")[[1]]
  
  bktrack <- lcsback(v, w)

  out <- vector('character')
  out <- outputlcs(bktrack, vvec, (vlen + 1), (wlen + 1), out)

  print(paste(rev(out), sep="", collapse=""))  
  
  return(bktrack)
}
