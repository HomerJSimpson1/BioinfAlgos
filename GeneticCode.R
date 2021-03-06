findntsinstring <- function(text="", pattern, infname="C:/Users/petersj/Downloads/dataset_96_8.txt",
                            outfname="C:/Users/petersj/Downloads/output.txt", usefile=TRUE, writefile=TRUE) {

  if(usefile)
    text <- readfiletostring(infname)
  nts <- findnucleotides(pattern)

  ntpats <- vector('character')
  nummatches <- vector('numeric')
  result <- vector('character')
  
  for (i in 1:ncol(nts)) {
    for (j in 1:nrow(nts)) {
      ntpats <- c(ntpats, nts[j, i])
      nmatches <- findnummatches(text, nts[j,i])
      nummatches <- c(nummatches, nmatches)
      if (nmatches > 0)
        {
          str <- rep(nts[j, i], nmatches)
          result <- c(result, str)
        }
      
    }
  }

  result <- trim(result)
  options(stringsAsFactors=FALSE)
  final <- as.data.frame(cbind(ntpats, nummatches))
  final$nummatches <- as.numeric(final$nummatches)

  if(writefile)
    writestringtofile(outfname, result)
  
  return(result)
}



trim <- function(x) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
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
  if (is.vector(string))
    {
      string <- paste(string, "", sep=" ", collapse="")
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)
}





findnummatches <- function(string, sequence) {
  match <- gregexpr(paste("(?=", sequence, ")", sep=""), string, perl=TRUE)

  if(match[[1]][1] == -1)
    nmatches <- 0
  else
    nmatches <- length(match[[1]])
  return(nmatches)
}






findnucleotides <- function(aastring) {
  ## Given a string of amino acids, return vectors of possible corresponding
  ## nucleotides.

  ## This needs a data structure that keeps the nucleotides together differently,
  ## and comes up with all combinations.  e.g. MA could match ATG with GCA
  ## i.e. ATGGCA, or it could match up ATG with GCC, i.e. ATGGCC, etc.  Also
  ## must handle the reverse complements. e.g. ATG reverse complement is CAT,
  ## and GCA reverse complement is TGC, so another pattern to search is
  ## CATTGC.  etc.
  
  ntvec <- vector('character')
  fwd <- vector('character')
  aavec <- strsplit(aastring,"")[[1]]
  codontable <- geneticcode()
  
  for (i in 1:length(aavec))
    {
      nts <- vector('character')      
      indices <- which(codontable$singleletters %in% aavec[i])

      for (j in 1:length(indices)) {
        ## iterate through all the matches
        ntmatch <- codontable$strng[indices[j]]
        ntmatch <- gsub("U", "T", ntmatch)
        fwd <- c(fwd, ntmatch)
      }
      nts <- as.data.frame(fwd)
      names(nts) <- NULL
      fwd <- vector('character')
      ntvec <- c(ntvec, nts)
      }

  final <- makepermuts(ntvec)
  final$reverse <- getreversecomps(final$forward)
  print(head(final))
  return(final)
}



getreversecomps <- function(fwdvec) {
  result <- sapply(fwdvec, function(x) { findrevcomp(x) })
  return(result)
}


makepermuts <- function(df) {
  ## Function that takes as input a vector and outputs a data frame that is the combination of
  ## the forward elements. e.g. create all permutations of fwd1 elements and fwd2 elements and
  ## fwd3 elements (if present), etc.
  ## E.g. if fwd1="ATG" and fwd2 = "GCA GCC GCG GCT", then create a "forward" vector of
  ## "ATGGCA", "ATGGCC", "ATGGCG", and "ATGGCT".
  ## Then return the two vector (forward) as a data frame.

  forward <- vector('character')
  result <- vector('character')
  numcols <- length(df)
  
  for (i in 1:numcols) {  
    ## Iterate through all of the columns    
    result <- mergevec(unlist(result), df[i])    
  }
  forward <- c(forward, result)
  
  ## Return the result
  return(as.data.frame(forward))
}




mergevec <- function(vec1, vec2) {
  ## "Merge" two vectors by creating all possible combinations of their elements.
  ## This process will treat them as strings and concatenate them to "merge" them.
  ## e.g. if vec1 = c(1, 2, 3) and vec2 = (4, 5, 6), then merge(vec1, vec2)
  ## should produce c(14, 15, 16, 24, 25, 26, 34, 35, 36)

  result <- vector('character')

  ## Check if vector1 is empty.
  if (length(vec1) == 0) {
    return(vec2)
  }   ## Check if vector2 is empty.
  else if (length(vec2) == 0) {
    return(vec1)
  }

  ## Create all possible combinations from the elements in each of the two vectors, or
  ## "merge" them as I referred to it here.
  for (i in 1:length(vec1)) {
    for (j in 1:length(vec2)) {      
      element <- paste(unlist(vec1[i]), unlist(vec2[j]), sep="")
      result <- c(result, element)
    }
  }

  return(result)
}



findabbrev <- function(nucleotidestring) {
  ## Given a pattern of nucleotides, returns the single letter abbreviation
  ## for each amino acid in the string.
  ## e.g. "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA" returns "MAMAPRTEINSTRING".
  ## Note that the stop codon returns no value.

  aastring <- vector('character')
  codontable <- geneticcode()
  codons <- splitstring2(nucleotidestring, 3)
  for(i in 1:length(codons)) {
    indx <- match(codons[i], codontable$strng, nomatch = -1)
    if (indx > 0) {
      aastring <- c(aastring, codontable$singleletters[indx])
    }
  }

  aastring <- paste(aastring,"",sep="",collapse="")
  return(aastring)
}


splitstring2 <- function(string, numchars=3) {
  result <- strsplit(string, paste("(?<=.{",numchars,"})",sep=""), perl = TRUE)[[1]]
  return(result)
}


geneticcode <- function() {
  ## Uses the alphabet A, C, T, U.

  options(stringsAsFactors=FALSE)
  singleletters <- vector('character')

  aminoacids <- as.data.frame(cbind(indx=0:63,strng=genperms(3)$strng))
  for (i in 1:nrow(aminoacids)) {
    nextpattern <- aminoacids$strng[i]
    singleletter <- getoneltrabbrev(nextpattern)
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
  #print(outputSeq)

  return(outputSeq)
}
  
reversestring <- function(inputString) {
  ## Reverse an input string
  splitInput <- strsplit(inputString,NULL)[[1]]
  revSplit <- rev(splitInput)
  outputString <- paste(revSplit,"",collapse='', sep='')
  return(outputString)
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
