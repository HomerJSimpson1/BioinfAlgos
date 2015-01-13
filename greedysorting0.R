greedysorting <- function(Pstring) {
  ## Input: A permutation P.
  ## Output: The sequence of permutations corresponding to applying GREEDYSORTING to P, ending with
  ## the identity permutation

  ## Sample Input:
  ##    (-3 +4 +1 +5 -2)
  ##
  ## Sample Output:
  ##    (-1 -4 +3 +5 -2)
  ##    (+1 -4 +3 +5 -2)
  ##    (+1 +2 -5 -3 +4)
  ##    (+1 +2 +3 +5 +4)
  ##    (+1 +2 +3 -4 -5)
  ##    (+1 +2 +3 +4 -5)
  ##    (+1 +2 +3 +4 +5)

  P <- readP(Pstring)

  fname <- "greedy.txt"
  #fcon <- file(fname)
  
  ## aprevdist = Approximate Reversal Distance
  apprevdist <- 0
  #P_output <- paste(P, "\n", sep="")
  #printP(P, fcon)

  ## No - don't include the input as the first line of the output.
  #printP(P, fname)  

  plen <- length(P)
  for (k in 1:plen) {
    ## if element k is not sorted
    if (abs(P[k]) != k) {
      i = k + 1
      while (abs(P[i]) != k) {
        ## while we haven't found the kth element (i.e. the element that "belongs" at P[k]),
        ## increment the variable i.
        i <- i + 1
      } ## end while (abs(P[i]) != k)
      
      ## Now we need to flip the subvector of P, starting at k and ending at i.
      ## We have to exchange the elements and flip their signs as well.
      left = k
      right = i
      
      while (right > left) {
        ## Flip P[left] and P[right] (and their signs)
        ## Then increment left and decrement right while right > left
        temp <- P[left]
        P[left] = -P[right]
        P[right] <- -temp
        
        left <- left + 1
        right <- right - 1
      } ## end while (right > left)
      
      if (left == right) {
        ## Then there were an odd number of elements from k to i
        ## Just flip the sign of the element where left == right
        P[left] = - P[left]
      }
      
      #P_output <- paste(P_output, P, "\n", sep="")
      apprevdist <- apprevdist + 1
      #print(P)
      #printP(P, fcon)
      printP(P, fname)      
    } ## end if (abs(P[k]) != k)

    if (P[k] == -k) {
      #print(k)
      #print(P[k])
      P[k] = k
      #print(k)
      #print(P[k])
      apprevdist <- apprevdist + 1
      #print(P)
      #printP(P, fcon)
      printP(P, fname)      
    } ## end if (P[k] == -k])
    
  } ## end for (k in 1:plen)

  #print(paste("Approx Reversal Distance = ", apprevdist, sep = ""))
  #print(P)
  #return(P_output)

  ## Close the file connection
  #close(fcon)
  
  return(apprevdist)
}




printP <- function(P, fname) {
  ## Print the P vectorlines to a file
  nums <- paste(sprintf("%+d", P), "", collapse="")
  trimd <- substr(nums, 1, nchar(nums) - 1)
  strOut <- paste("(", trimd, ")", sep="")
  #print(strOut)

  write(strOut, fname, append=TRUE)

}




readP <- function(input) {
  ## Convert input into a vector

##   temp <- substr(input, 2, nchar(input) - 1)
##   temp1 <- strsplit(temp, " ")[[1]]
##   temp2 <- as.integer(temp1)

  temp <- as.integer(strsplit(substr(input, 2, nchar(input) - 1), " ")[[1]])
  
  return(temp)
}






## printP <- function(P, fcon) {
##   ## Print the P vectorlines to a file
##   nums <- paste(sprintf("%+d", P), "", collapse="")
##   trimd <- substr(nums, 1, nchar(nums) - 1)
##   strOut <- paste("(", trimd, ")", sep="")
##   print(strOut)

##   writeLines(strOut, fcon)

  
## ##   for (i in 1:length(P)) {
## ##     str <- sprintf("%+d", P)
    
## ##   }


## }




