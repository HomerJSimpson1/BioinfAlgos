permutestring <- function(pattern, hammingd) {
  ## Function to create a vector of all strings drawn from an alphabet that are within a
  ## a Hamming Distance d (here the input parameter is hammingd) from the input string "pattern".

  alphabet = c('A', 'C', 'G', 'T')
  #alphaLength <- length(alphabet)
  ## newAlphaLength represents the length of the alphabet minus one character
  ## (fixed by pattern for each position).
  newAlphaLength <- length(alphabet) - 1

  #mutants <- vector('character')
  mutants <- c(pattern)
  #print(mutants)

  patternLength <- nchar(pattern)
  pattern <- toupper(pattern)  ## Make sure we're not mixing case

  #for(l in 1:nchar(pattern) - 1) {
  
  for(i in 1:(patternLength - 1)) {
    #mutant <- pattern
    #outeralpha <- removeletter(alphabet, substring(mutant, i, i))
    outeralpha <- removeletter(alphabet, substring(pattern, i, i))
    #print(outeralpha)

    
    for (j in (i+1):patternLength ) {
      ## for (j in i+1:newAlphaLength ) {
      # mutant1 <- pattern
      # inneralpha <- removeletter(alphabet, substring(mutant1, j, j))
      inneralpha <- removeletter(alphabet, substring(pattern, j, j))
      #print("Inner loop: ")
      #print(inneralpha)

      # I tested the following in R and it worked.
      # foo <- function(i,x,y) { temp <- strsplit(x,"")[[1]]; print(temp); temp[i] <- y; return(paste(temp,collapse=""))}
      # pattern1 <- "ACGT"
      # replacements <- strsplit("ACT", "")
      # newx1 <- sapply(replacements[[1]], function(y) { foo(3, pattern1, y)})
      # names(newx1) = NULL
      # newx1 is now a vector that contains the elements "ACAT" "ACCT" "ACTT"

      mutant <- strsplit(pattern,"")[[1]]
      for(l in 1:newAlphaLength) {
        mutant[i] <- outeralpha[l]
        paste(mutant,collapse="")
        print(mutant)
        newx1 <- sapply(inneralpha, function(y) { foo(j, mutant, y)})
        names(newx1) <- NULL
        mutants <- c(mutants, newx1)
      } ## End of for(l in 1:newAlphaLength)
      
    } ## End of for(j in (i+1):patternLength)
    
  } ## End of for(i in 1:(patternLength - 1))


  print(length(mutants))
  return(mutants)

}


## Replaces element i of x with the character specified by y
foo <- function(i,x,y) { temp <- strsplit(x,"")[[1]]; temp[i] <- y; return(paste(temp,collapse=""))}
#foo <- function(i,x,y) { temp <- strsplit(x,"")[[1]]; print(temp); temp[i] <- y; return(paste(temp,collapse=""))}


    
removeletter <- function(alphabet, letter) {
  ## Remove the letter given by "letter" from the alphabet given by "alphabet."
  ## Return the new alphabet minus the specified letter.
  newAlpha <- alphabet[which(alphabet != letter)]
  return(newAlpha)
}
