findrevcomp <- function(inputSeq) {
  ## Given a sequence, return the complementary strand (given in its order of 5' -> 3').
  ## So, first find the complementary nucleotide for each nucleotide of the input strandSeq
  ## and then reverse that string once it's built.

  outputSeq = vector('character')
  lenInputSeq = nchar(inputSeq)

  #splitInput <- strsplit(inputSeq,NULL)[[1]]
  #print(splitInput)
  #lenInputSeq = length(splitInput)
  #print(lenInputSeq)
  
  for(i in 1:lenInputSeq)
    {
      #print(i)
      #print(substring(splitInput[i], i, i+1))
      #print(substring(inputSeq, i, i))

      if(substring(inputSeq, i, i) == 'A')
        outputSeq <- c(outputSeq, 'T')
      if(substring(inputSeq, i, i) == 'T')
        outputSeq <- c(outputSeq, 'A')
      if(substring(inputSeq, i, i) == 'G')
        outputSeq <- c(outputSeq, 'C')
      if(substring(inputSeq, i, i) == 'C')
        outputSeq <- c(outputSeq, 'G')

      
      ## if(substring(splitInput[i], i, i) == 'A')
      ##   outputSeq <- c(outputSeq, 'T')
      ## if(substring(splitInput[i], i, i) == 'T')
      ##   outputSeq <- c(outputSeq, 'A')
      ## if(substring(splitInput[i], i, i) == 'G')
      ##   outputSeq <- c(outputSeq, 'C')
      ## if(substring(splitInput[i], i, i) == 'C')
      ##   outputSeq <- c(outputSeq, 'G')

      #print(outputSeq)
      #rint(i)
    }

  #print(outputSeq)
  
  outputSeq <- paste(outputSeq, "", collapse='', sep='')

  outputSeq <- reversestring(outputSeq)
  print(outputSeq)

  return(outputSeq)
}
  
reversestring <- function(inputString) {
  ## Reverse an input string

  splitInput <- strsplit(inputString,NULL)[[1]]
  revSplit <- rev(splitInput)
  outputString <- paste(revSplit,"",collapse='', sep='')
  return(outputString)
}
