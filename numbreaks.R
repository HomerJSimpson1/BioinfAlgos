numbreaks <- function(Pstring) {
  ## Input: A permutation
  ## Output: The number of breakpoints in this permutation.

  ## Sample Input:
  ##    (+3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14)
  ##
  ## Sample Output:
  ##    8

  ## Convert Pstring into an integer vector P (see the greedysorting.R file)


  ## Create a grouping of each pair of adjacent elements to see if they are an adjacency or a breakpoint.
  ## Or just do it in place in the vector?



  return(breakpoints)
}
