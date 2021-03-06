findrevcomp <- function(inputSeq) {
  ## Given a sequence, return the complementary strand (given in its order of 5' -> 3').
  ## So, first find the complementary nucleotide for each nucleotide of the input strandSeq
  ## and then reverse that string once it's built.

  outputSeq = vector('character')
  lenInputSeq = nchar(inputSeq)

  for(i in 1:lenInputSeq)
    {
      if(substring(inputSeq, i, i) == 'A')
        outputSeq <- c(outputSeq, 'T')
      if(substring(inputSeq, i, i) == 'T')
        outputSeq <- c(outputSeq, 'A')
      if(substring(inputSeq, i, i) == 'G')
        outputSeq <- c(outputSeq, 'C')
      if(substring(inputSeq, i, i) == 'C')
        outputSeq <- c(outputSeq, 'G')
    }

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
