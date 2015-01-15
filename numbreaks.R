numbreaks <- function(Pstring="", isfile=FALSE, fname="") {
  ## Input: A permutation.  If isfile == TRUE, then Pstring designates a file containing Pstring (the permutation).
  ##        If isfile == FALSE, then Pstring is the raw string representing the permutation.
  ## Output: The number of breakpoints in this permutation.

  ## Sample Input:
  ##    (+3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14)
  ##
  ## Sample Output:
  ##    8

  ## if the permutation needs to be read from a file, then read in the file into Pstring
  if (isfile) {
    fname <- Pstring
    Pstring1 <- readfiletostring(fname)
    ## Convert Pstring into an integer vector P (see the greedysorting.R file)
    P <- readP(Pstring1)
  }
  else   ## Convert Pstring into an integer vector P (see the greedysorting.R file)
    P <- readP(Pstring)

  ##print(str(Pstring1))
    
  ## ## Convert Pstring into an integer vector P (see the greedysorting.R file)
  ##P <- readP(Pstring)

  ## Add a "0" element and an "n + 1" element
  #print(max(P))
  P1 <- c(0, P, length(P) + 1)
  #print(P1)  

  print(length(P))
  print(length(P1))
  
  ## Create a grouping of each pair of adjacent elements to see if they are an adjacency or a breakpoint.
  ## Or just do it in place in the vector?
  adjbreak <- vector('character')
  for (i in 1:(length(P1) - 1)) {
    ## iterate through the P1 vector and compare each two adjacent elements to determine if
    ## if they are an adjacency or a break.
    #print(P1[i])
    #print(P1[i + 1])
    if (P1[i+1] == (P1[i] + 1)) {
      adjbreak <- c(adjbreak, 'adj')
    }
    else {
      adjbreak <- c(adjbreak, 'break')
    }
    
  } ## end for (i in 1:(length(P1) - 1))

  breakpoints <- adjbreak[adjbreak=='break']
  adjacencies <- adjbreak[adjbreak=='adj']
  print(adjacencies)

  numbrk <- length(breakpoints)
  numadj <- length(adjacencies)

  print(numbrk)
  print(numadj)
  
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




## readfiletovec <- function(filename, header=FALSE, stringsAsFactors=FALSE) {
##   ## Read the contents of a file into a vector
##   ## Each line in the file corresponds to a single element of the vector.
##   vec <- read.table(filename, header=header, stringsAsFactors=stringsAsFactors)
##   return(vec[,1])
## }




readfiletostring <- function(filename, usespace=FALSE, nocollapse=FALSE) {
  ## Read the contents of a file into a single string.
  if(usespace)
    if(nocollapse)
      result <- paste(readLines(filename))
    else
      result <- paste(readLines(filename), collapse=" ")
  else
    if(nocollapse)
      result <- paste(readLines(filename))
    else
      result <- paste(readLines(filename), collapse="")

  return(result)
}
