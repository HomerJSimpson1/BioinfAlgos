findntsinstring <- function(text, pattern) {
  nts <- findnucleotides(pattern)
  for (i in 1:length(nts)) {
    
  }
}


callfindnt <- function(aastring) {
  ## Call findnucleotides function
  ## For some strange reason, I was unable to perform
  ## the following post-processing work within findnucleotides.
  ## So I wrapped it with this function.

  mylist <- findnucleotides(aastring)
  #ntlist[[1]][[1]] <- ntlist[[1]][[2]]
  ## Remove the empty list (list()) at the head of the list
  ## Replace it with the first entry, which is at [[1]][[2]]  
  ntlist[[1]][[1]] <- ntlist[[1]][[2]]
  ## # Now remove the entry at [[1]][[2]]
  ntlisttmp <- ntlist[[1]][[-2]]
  ## # And clean it up to have the list I want
  ntlist <- list(ntlisttmp, ntlist[[-1]])

  return(ntlist)
}
  

findnucleotides <- function(aastring) {
  ## Given a string of amino acids, return vectors of possible corresponding
  ## nucleotides.

  ## This is wrong.  It needs to keep the nucleotides together differently,
  ## and come up with all combinations.  e.g. MA could match ATG with GCA
  ## i.e. ATGGCA, or it could match up ATG with GCC, i.e. ATGGCC, etc.  Also
  ## must handle the reverse complements. e.g. ATG reverse complement is CAT,
  ## and GCA reverse complement is TGC, so another pattern to search is
  ## CATTGC.  etc.  Fix it!
  
  #ntlist <- list()
  ntvec <- vector('character')
  fwd <- vector('character')
  rev <- vector('character')
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
        #print(ntmatch)
        #nts <- c(nts, ntmatch)
        ntmatchrev <- findrevcomp(ntmatch)

        fwd <- c(fwd, ntmatch)
        rev <- c(rev, ntmatchrev)
        
        #nti <- cbind(fwd=ntmatch, rev=ntmatchrev)
        #nti <- as.data.frame(cbind(fwd=ntmatch, rev=ntmatchrev))
        #nts <- c(nts, nti)
        #print(nts)
        #nts <- c(nts, ntmatch, ntmatchrev)
      }
      nts <- as.data.frame(cbind(fwd, rev))
      colnames(nts) <- c(paste('fwd', i, sep=""), paste('rev', i, sep=""))
      #ntlist <- append(ntlist, nts)
      #ntlist <- list(ntlist, nts)
      
      ntvec <- c(ntvec, nts)
    }

  #return(ntlist)
  return(ntvec)
}


getnt <- function(aminoacid, codontable) {
  #logvec <- aminoacid %in% codontable$singleletters
  #print(logvec)
  #indx <- which(aminoacid %in% codontable$singleletters)
  indx <- which(codontable$singleletters %in% aminoacid)
  #print(indx)
}


findabbrev <- function(nucleotidestring) {
  ## Given a pattern of nucleotides, returns the single letter abbreviation
  ## for each amino acid in the string.
  ## e.g. "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA" returns "MAMAPRTEINSTRING".
  ## Note that the stop codon returns no value.

  aastring <- vector('character')
  codontable <- geneticcode()
  #print(head(codontable))
  #print(str(codontable))
  codons <- splitstring2(nucleotidestring, 3)
  for(i in 1:length(codons)) {
    #print(codons[i])
    indx <- match(codons[i], codontable$strng, nomatch = -1)
    if (indx > 0) {
      #print(indx)
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








## callfindnt <- function(aastring) {
##   ## Call findnucleotides function
##   ## For some strange reason, I was unable to perform
##   ## the following post-processing work within findnucleotides.
##   ## So I wrapped it with this function.

##   mylist <- findnucleotides(aastring)
##   #ntlist[[1]][[1]] <- ntlist[[1]][[2]]
##   ntlist[[1]][[1]] <- ntlist[[1]][[2]]
##   ## # Now remove the entry at [[1]][[2]]
##   ntlisttmp <- ntlist[[1]][[-2]]
##   ## # And clean it up to have the list I want
##   ntlist <- list(ntlisttmp, ntlist[[-1]])

##   return(ntlist)
## }
  

## findnucleotides <- function(aastring) {
##   ## Given a string of amino acids, return vectors of possible corresponding
##   ## nucleotides.

##   ## This is wrong.  It needs to keep the nucleotides together differently,
##   ## and come up with all combinations.  e.g. MA could match ATG with GCA
##   ## i.e. ATGGCA, or it could match up ATG with GCC, i.e. ATGGCC, etc.  Also
##   ## must handle the reverse complements. e.g. ATG reverse complement is CAT,
##   ## and GCA reverse complement is TGC, so another pattern to search is
##   ## CATTGC.  etc.  Fix it!
  
##   ntlist <- list()
##   #ntvec <- vector('character')
##   aavec <- strsplit(aastring,"")[[1]]
##   codontable <- geneticcode()
  
##   for (i in 1:length(aavec))
##     {
##       nts <- vector('character')      
##       indices <- which(codontable$singleletters %in% aavec[i])

##       for (j in 1:length(indices)) {
##         ## iterate through all the matches
##         ntmatch <- codontable$strng[indices[j]]
##         ntmatch <- gsub("U", "T", ntmatch)
##         #print(ntmatch)
##         #nts <- c(nts, ntmatch)
##         ntmatchrev <- findrevcomp(ntmatch)
##         nts <- c(nts, ntmatch, ntmatchrev)
##       }
##       #ntlist <- append(ntlist, nts)
##       ntlist <- list(ntlist, nts)
##       # Remove the empty list (list()) at the head of the list
##       # Replace it with the first entry, which is at [[1]][[2]]
##       #ntlist[[1]][[1]] <- ntlist[[1]][[2]]
##       #print(ntlist[[1]][2])
##       #temp <- ntlist[[1]]
##       #print(temp[2][[1]])
##       ## ntlist[[1]][[1]] <- ntlist[[1]][[2]]
##       ## # Now remove the entry at [[1]][[2]]
##       #ntlisttmp <- ntlist[[1]][[-2]]
##       ## # And clean it up to have the list I want
##       #ntlist <- list(ntlisttmp, ntlist[[-1]])
##       #ntvec <- c(ntvec, nts)
##     }

##   return(ntlist)
##   #return(ntvec)
## }







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
