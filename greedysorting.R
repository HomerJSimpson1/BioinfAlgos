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

  plen <- length(P)
  for (k in 1:plen) {
    ## if element k is not sorted
    
  }
  
}
