findabbrev <- function(nucleotidestring) {
  ## Given a pattern of nucleotides, returns the single letter abbreviation
  ## for each amino acid in the string.
  ## e.g. "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA" returns "MAMAPRTEINSTRING".
  ## Note that the stop codon returns no value.

  aastring <- vector('character')
  codontable <- geneticcode()
  #print(head(codontable))
  print(str(codontable))
  codons <- splitstring2(nucleotidestring, 3)
  for(i in 1:length(codons)) {
    print(codons[i])
    indx <- match(codons[i], codontable$strng, nomatch = -1)
    if (indx > 0) {
      print(indx)
      aastring <- c(aastring, codontable$singleletters[indx])
    }
  }

  aastring <- paste(aastring,"",sep="",collapse="")
  return(aastring)
}


splitstring <- function(string, numchars) {
  ## Returns the string as a vector of substrings, all of length numchars.
  ## Klunky...

  starts <- seq(1,nchar(string), by=numchars)
  result <- sapply(starts, function(iii) { substr(string, iii, iii + (numchars - 1))})
  return(result)
}



splitstring2 <- function(string, numchars=3) {
  ## Same as splitstring(), but different methodology.
  #gregexpr and regmatches:

  ## Unfortunately, this first one drops any tail elements that form a group < numchars.
  ## i.e. if the string length is 21 and numchars = 6, then the last 3 characters are dropped.
  #result <- regmatches(string, gregexpr(paste(".{",numchars,"}",sep=""), string))[[1]] 
  ## OR can also use the below
  
  result <- strsplit(string, paste("(?<=.{",numchars,"})",sep=""), perl = TRUE)[[1]]
  return(result)
}


geneticcode <- function() {
  ## Uses the alphabet A, C, T, U.

  options(stringsAsFactors=FALSE)
  #singleletterabbrev <- c('K', 'N', 'K', 'N',
  singleletters <- vector('character')

  aminoacids <- as.data.frame(cbind(indx=0:63,strng=genperms(3)$strng))
  #print(nrow(aminoacids))
  for (i in 1:nrow(aminoacids)) {
    nextpattern <- aminoacids$strng[i]
    #print(nextpattern)
    singleletter <- getoneltrabbrev(nextpattern)
    #print(singleletter)
    singleletters <- c(singleletters, singleletter)
  }
  aminoacids <- cbind(aminoacids, singleletters)
  aminoacids$indx <- as.numeric(aminoacids$indx)
  return(aminoacids)
}


getoneltrabbrev <- function(aminoacid) {
  ## Return the single letter abbreviation (e.g. "K" for Lysine) for the given amino acid.
  ## The input aminoacid is given as its constituent nucleotides, e.g. "AAA".

  if (aminoacid %in% c("GCU", "GCC", "GCA", "GCG"))
    return("A")
  else if (aminoacid %in% c("UGU", "UGC"))
    return("C")  
  else if (aminoacid %in% c("GAU", "GAC"))
    return("D")
  else if (aminoacid %in% c("GAA", "GAG"))
    return("E")  
  else if (aminoacid %in% c("UUU", "UUC"))
    return("F")
  else if (aminoacid %in% c("GGU", "GGC", "GGA", "GGG"))
    return("G")
  else if (aminoacid %in% c("CAU", "CAC"))
    return("H")
  else if (aminoacid %in% c("AUU", "AUC", "AUA"))
    return("I")
  else if (aminoacid %in% c("AAA", "AAG"))
    return("K")
  else if (aminoacid %in% c("UUA", "UUG", "CUU", "CUC", "CUA", "CUG"))
    return("L")  
  else if (aminoacid %in% c("AUG"))
    return("M")
  else if (aminoacid %in% c("AAU", "AAC"))
    return("N")
  else if (aminoacid %in% c("CCU", "CCC", "CCA", "CCG"))
    return("P")
  else if (aminoacid %in% c("CAA", "CAG"))
    return("Q")
  else if (aminoacid %in% c("CGU", "CGC", "CGA", "CGG", "AGA", "AGG"))
    return("R")
  else if (aminoacid %in% c("UCU", "UCC", "UCA", "UCG", "AGU", "AGC"))
    return("S")
  else if (aminoacid %in% c("ACU", "ACC", "ACA", "ACG"))
    return("T")
  else if (aminoacid %in% c("GUU", "GUC", "GUA", "GUG"))
    return("V")
  else if (aminoacid %in% c("UGG"))
    return("W")
  else if (aminoacid %in% c("UAU", "UAC"))
    return("Y")
  else if (aminoacid %in% c("UAA", "UGA", "UAG"))
    return("")
    #return("*")
  
}



foo <- function(dfrow) {
  ## Paste together the column values in a row of a data frame.
  #stringres <- sapply(1:ncol(dfrow), paste0(dfrow, collapse=""))
  stringres <- paste0(dfrow, collapse="")
  return(stringres)
}


genperms <- function(k) {
  ## Generate all permutations of length k for a given alphabet.

  ## Assuming an alphabet here of {"A", "C", "G", "U"}
  alphabet = c('A', 'C', 'G', 'U')

  vects <- vector('character')

  for (i in 1:k) {
    vectA <- rep("A", 4^(k - i))
    vectC <- rep("C", 4^(k - i))
    vectG <- rep("G", 4^(k - i))
    vectU <- rep("U", 4^(k - i))
    vect <- c(vectA, vectC, vectG, vectU)
    vects <-cbind(vects, vect)
  }

  df <- data.frame(vects, stringsAsFactors = FALSE)
  df$strng <- apply(df, 1, function(x) {foo(x)})
  
  return(df)
}




gettable <- function(AApattern) {

  table <- ("AAA K
AAC N
AAG K
AAU N
ACA T
ACC T
ACG T
ACU T
AGA R
AGC S
AGG R
AGU S
AUA I
AUC I
AUG M
AUU I
CAA Q
CAC H
CAG Q
CAU H
CCA P
CCC P
CCG P
CCU P
CGA R
CGC R
CGG R
CGU R
CUA L
CUC L
CUG L
CUU L
GAA E
GAC D
GAG E
GAU D
GCA A
GCC A
GCG A
GCU A
GGA G
GGC G
GGG G
GGU G
GUA V
GUC V
GUG V
GUU V
UAA 
UAC Y
UAG 
UAU Y
UCA S
UCC S
UCG S
UCU S
UGA 
UGC C
UGG W
UGU C
UUA L
UUC F
UUG L
UUU F
")
}
