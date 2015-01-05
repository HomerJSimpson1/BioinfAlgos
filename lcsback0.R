lcsback <- function(v, w) {
  ## Input: 2 strings
  ## Output: The path through the graph creating the
  ## longest common subsequence.

  #backtrack <- vector('character')
##   vlen <- length(v)
##   wlen <- length(w)
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
  #for (i in 1:vlen) {    
    #print(vvec[i])    
    for (j in 2:(wlen + 1)) {
    #for (j in 1:wlen) {      
      #print(wvec[j])
      #if (vvec[i] == wvec[j]){
      if (vvec[i-1] == wvec[j-1]){        
        s[i, j] <- max(s[i-1, j], s[i, j-1], s[i-1, j-1] + 1)
      }
      else {
        s[i, j] <- max(s[i-1, j], s[i, j-1])
      } ## end else

      #print(paste("i=", i, "j=", j, "s[i, j] =", s[i, j], "s[i-1, j-1] =", s[i-1, j-1]), sep=" ")      
      if (s[i, j] == s[(i-1), j])
        backtrack[i, j] <- 'd'
      else if (s[i, j] == s[i, (j-1)])
        backtrack[i, j] <- 'r'
      else if (s[i, j] == (s[(i-1), (j-1)] + 1)) {
        #print(paste("s[i, j] =", s[i, j], "s[i-1, j-1] =", s[(i-1), (j-1)]), sep=" ")
        backtrack[i, j] <- 'g'
      }
        
    } ## end for (j in 2:(wlen + 1))
  } ## end for (i in 2:(vlen + 1))

  print(s)

  
##   ###outputlcs(backtrack, v, vlen, wlen)
##   #outputlcs(backtrack, vvec, vlen, wlen)
##   out <- vector('character')

##   ## On larger strings, this recursive function crashes!
##   ## Increase number of recursive calls allowed using options(expressions=..)
##   options(expressions=20000)
##   #out <- outputlcs(backtrack, vvec, vlen, wlen, out)
  
##   ## I think the above works correctly, but the outputlcs function does not...
##   out <- outputlcs(backtrack, vvec, (vlen + 1), (wlen + 1), out)
##   #outputlcs(backtrack, vvec, vlen, wlen)

##   #print(out)
  
  return(backtrack)
}





outputlcs <- function(backtrack, v, i, j, outpt) {
  ## Input: backtrack matrix, character vector v,
  ## i, and j
  ## Output: the longest common subsequence (LCS)

  ## while (i > 0 && j > 0) {
  while (i > 1 && j > 1) {
    #print(paste("i=", i, "j=", j, sep=" "))
    if (backtrack[i, j] == 'd') {
      i = i - 1
      #print("Going up one row")
    }
    else if (backtrack[i, j] == 'r') {
      j = j - 1
      #print("Going left one column")
    }
    else if (backtrack[i, j] == 'g') {
      #outpt <- c(outpt, v[i])      
      i = i - 1
      j = j - 1
      #print("Going up one row and left one column")
      outpt <- c(outpt, v[i])
    }

  } ## end while loop

  return(outpt)
}
  



## outputlcs <- function(backtrack, v, i, j, outpt) {
##   ## Input: backtrack matrix, character vector v,
##   ## i, and j
##   ## Output: the longest common subsequence (LCS)

##   #if (i == 0 || j == 0) {
##   if (i == 1 || j == 1) {    
##     #print("returning from function")
##     print(paste(rev(outpt), sep="", collapse=""))
##     #return("")
##   }
##   else if (backtrack[i, j] == 'd') {
##     print(paste("i=", i, "j=", j, sep=" "))
##     outputlcs(backtrack, v, i - 1, j, outpt)
##   }
##   else if (backtrack[i, j] == 'r') {
##     print(paste("i=", i, "j=", j, sep=" "))
##     outputlcs(backtrack, v, i, j - 1, outpt)
##   }
##   else {
##     outpt <- c(outpt, v[i])    
##     outputlcs(backtrack, v, i - 1, j - 1, outpt)
##     #print(v[i])    
##   }

##   #return(outpt)
## }





## outputlcs <- function(backtrack, v, i, j) {
##   ## Input: backtrack matrix, character vector v,
##   ## i, and j
##   ## Output: the longest common subsequence (LCS)

##   #if (i == 0 || j == 0) {
##   if (i == 1 || j == 1) {    
##     #print("returning from function")
##     #print(paste(rev(outpt), sep="", collapse=""))
##     return("")
##   }
##   else if (backtrack[i, j] == 'd')
##     outputlcs(backtrack, v, i - 1, j)
##   else if (backtrack[i, j] == 'r')
##     outputlcs(backtrack, v, i, j - 1)
##   else {
##     outputlcs(backtrack, v, i - 1, j - 1)
##     print(v[i])
##   }
  
## }




calllcsback <- function(v, w) {
  vlen <- nchar(v)
  wlen <- nchar(w)
  vvec <- strsplit(v, "")[[1]]
  
  bktrack <- lcsback(v, w)

  out <- vector('character')
  #options(expressions=20000)
  
  out <- outputlcs(bktrack, vvec, (vlen + 1), (wlen + 1), out)
  #print(out)
  print(paste(rev(out), sep="", collapse=""))  
  
  return(bktrack)
}
