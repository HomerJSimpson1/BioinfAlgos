findkmers3 <- function(string, L, k, t) {
  ## Find the number of occurences of pattern in string.
  ## This version works correctly.

  #patLength <- nchar(pattern) ## This is k in this function
  stringLength <- nchar(string)

  count <- 0
  #indices <- vector('numeric')
  patterns <- vector('character')
  results <- cbind(vector('character'), vector('numeric'))
  #results <- cbind(vector('character'), vector('numeric'), vector('numeric'), vector('numeric'), vector('numeric'))
  #ws <- vector('numeric')
  #is <- vector('numeric')
  #js <- vector('numeric')

  for(w in 1:(stringLength - k + 1))  ## Correct?
    {
      #print(w)
      if((L + w - 1) > stringLength)
        stoppoint <- stringLength
      else
        stoppoint <- L + w - 1
      #stringseg <- substring(string, w, L + (w - 1))
      stringseg <- substring(string, w, stoppoint)
      #print(paste("w = ", w, ", stringseg = ", stringseg, sep=""))

      for(i in 1:(L - k + 1))
        {
          #print(substring(string, i, i + (patLength - 1)))
          #if(substring(string, i, i + (patLength - 1)) == pattern)
          pattern = substring(string, i, (i + k - 1))
          #patterns <- c(patterns, pattern)
          indices <- vector('numeric')
          indices <- c(indices, i)
          #for(j in (i+1):(stringLength - k)) {
          for(j in (i+1):(L - k)) {            
            if(substring(stringseg, j, (j + k - 1)) == pattern)
              {
                #indices <- c(indices, j)
                indices <- c(indices, (w + j - 1))
                #ws <- c(ws, w)
                #is <- c(is, i)
                #js <- c(js, j)
                #count <- count + 1
              }
          } ## End for (j ....)

          if(length(indices) >= t)
            {
              result <- cbind(pattern, indices)
              #result <- cbind(pattern, indices, ws, is, js)
              #print(result)
              
              results <- rbind(results, result)
              #patterns <- c(patterns, pattern)
              #indices <- c(indices, indices
            }
        } ## End for (i ...)
      
    } ## End for (w ...)
  
  #return(results)
  #return(unique(results))
  #return(unique(results$pattern))
  return(unique(results[, 1]))

}








    
      
  
##   nwindows <- ceiling(stringLength / L)
##   for(w in 1:nwindows)
##     {
##       if(w*L > stringLength)
##         {
##           stoppoint <- stringLength
##         }
##       else
##         {
##           stoppoint <- w*L
##         }
##      
##       stringseg <- substring(string, (w-1) * L + 1, stoppoint)
##       print(stringseg)
##    }
##
##      
##       for(i in 1:(L - k + 1))
##         {
##           #print(substring(string, i, i + (patLength - 1)))
##           #if(substring(string, i, i + (patLength - 1)) == pattern)
##           pattern = substring(string, i, i + k)
##           #patterns <- c(patterns, pattern)
##           indices <- vector('numeric')
##           indices <- c(indices, i)
##           for(j in (i+1):(stringLength - k)) {
##             if(substring(string, j, j + k) == pattern)
##               {
##                 indices <- c(indices, j)
##                 #count <- count + 1
##               }
##           } ## End for (j ....)
##
##           if(length(indices) > t)
##             {
##               result <- cbind(pattern, indices)
##               print(result)
##            
##               results <- rbind(results, result)
##               #patterns <- c(patterns, pattern)
##               #indices <- c(indices, indices
##             }
##         } ## End for (i ...)
##
##
##
##    
##    } ## End for (w ...)
##
##
##
##   #return(count)
##   return(results)
##
##}
