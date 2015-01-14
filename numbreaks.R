numbreaks <- function(Pstring) {
  ## Input: A permutation
  ## Output: The number of breakpoints in this permutation.

  ## Sample Input:
  ##    (+3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14)
  ##
  ## Sample Output:
  ##    8

  ## Convert Pstring into an integer vector P (see the greedysorting.R file)
  P <- readP(Pstring)

  ## Add a "0" element and an "n + 1" element
  P1 <- c(0, P, length(P) + 1)

  ## Create a grouping of each pair of adjacent elements to see if they are an adjacency or a breakpoint.
  ## Or just do it in place in the vector?

  print(P1)

  breakpoints <- 0
  
  return(breakpoints)
}







printP <- function(P, fname) {
  ## Print the P vectorlines to a file
  nums <- paste(sprintf("%+d", P), "", collapse="")
  trimd <- substr(nums, 1, nchar(nums) - 1)
  strOut <- paste("(", trimd, ")", sep="")
  write(strOut, fname, append=TRUE)
}



readP <- function(input) {
  ## Convert input into a vector
  temp <- as.integer(strsplit(substr(input, 2, nchar(input) - 1), " ")[[1]])  
  return(temp)
}
