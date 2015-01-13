greedysorting <- function(P) {
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

  ## aprevdist = Approximate Reversal Distance
  apprevdist <- 0
  P_output <- paste(P, "\n", sep="")

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
      
      P_output <- paste(P_output, P, "\n", sep="")
      apprevdist <- apprevdist + 1
      print(P)      
    } ## end if (abs(P[k]) != k)

    if (P[k] == -k) {
      #print(k)
      #print(P[k])
      P[k] = k
      #print(k)
      #print(P[k])
      apprevdist <- apprevdist + 1
      print(P)
    } ## end if (P[k] == -k])
    
  } ## end for (k in 1:plen)

  print(paste("Approx Reversal Distance = ", apprevdist, sep = ""))
  #print(P)
  return(P_output)
  #return(apprevdist)
}
