findkmers4 <- function(k, L, t, infilename="C:/Users/James User/Downloads/dataset_4_4.txt", outfilename="C:/Users/James User/Desktop/outfile.txt") {
  ##findkmers4 <- function(string, k, L, t, infilename, outfilename="C:/Users/James User/Desktop/outfile.txt") {  
  ## Find the number of occurences of pattern in string.
  ## This version works correctly.

  require(stringr)  ## Need for str_trim() function.

  string <- readfiletostring(infilename)
  
  #patLength <- nchar(pattern) ## This is k in this function
  stringLength <- nchar(string)

  count <- 0
  #indices <- vector('numeric')
  patterns <- vector('character')
  results <- cbind(vector('character'), vector('numeric'))
  result <- cbind(vector('character'), vector('numeric'))
  
  for(i in 1:(stringLength - k + 1))
    {
      #print(substring(string, i, i + (patLength - 1)))
      #if(substring(string, i, i + (patLength - 1)) == pattern)
      pattern = substring(string, i, (i + k - 1))
      #patterns <- c(patterns, pattern)
      indices <- vector('numeric')
      indices <- c(indices, i)
      for(j in (i+1):(stringLength - k)) {
        if(substring(string, j, (j + k - 1)) == pattern)
          {
            #print(pattern)
            indices <- c(indices, j)
            #count <- count + 1
          }
      } ## End for (j ....)

      if(length(indices) >= t)
         {
           #result <- cbind(pattern, indices)
           result <- list(pattern, indices)           
           #print(result)
           
           results <- rbind(results, result)
           #patterns <- c(patterns, pattern)
           #indices <- c(indices, indices
         }
      #results <- rbind(results, result)
    } ## End for (i ...)


  ## Find out which patterns have t occurences within distance L
  final <- vector('character')
  #numrows <- nrow(results)
  for(i in 1:nrow(results)) {
    numindices <- length(results[[i, 2]])
    if (numindices > t) {
      for (j in 1:(numindices - t + 1)) {
        diff <- results[[i, 2]][t + j - 1] - results[[i, 2]][j]
        if (diff <= L)
          final <- c(final, results[[i, 1]])
      }
    }
    else {
      diff <- results[[i, 2]][numindices] - results[[i, 2]][1]
      if(diff <= L)
        final <- c(final, results[[i, 1]])
    }
  } ## End for (i in 1:nrow(results))

  final <- unique(final)

  writestringtofile(outfilename, final)
  
  #return(count)
  #return(results)
  #return(unique(final))
  return(final)

}




readfiletostring <- function(filename, usespace=FALSE) {
  ## Read the contents of a file into a single string.
  if(usespace)
    result <- paste(readLines(filename), collapse=" ")
  else
    result <- paste(readLines(filename), collapse="")

  return(result)
}




writestringtofile <- function(filename, string) {
  ## Write a string to a file given by filename
#  print(filename)
  if (is.vector(string))
    {
      string <- paste(string, "", sep=" ", collapse="")
#      print(string)
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)

}







## ## Not useful
## makevec <- function(vec) {
##   ## Make a vector
##   strvec <- paste(vec,"",collapse="",sep=" ")
##   strvec <- str_trim(strvec)
##   vec2 <- as.integer(strsplit(strvec," ")[[1]])
##   return(vec2)
## }



## applyL <- function(resultpats, L) {
##   ## Find difference between the indices in resultpats
##   ## that are within distance L from each other.

##   if(resultpats[1] == pattern) {
##     #diff <- resultpats[]

##   }

## }
