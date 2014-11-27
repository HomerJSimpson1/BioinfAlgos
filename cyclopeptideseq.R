cyclopeptideseq <- function(spectrum) {
  ## Coursera class Bioinformatics Algorithms Part 1.
  ## This function uses a branch-and-bound style algorithm to find a peptide sequence from a
  ## given mass spectrum (via the input parameter "spectrum").

  ## Pseudocode from the interactive e-text is as follows:
  ## CYCLOPEPTIDESEQUENCING(Spectrum)
  ##      Peptides = {empty peptide}
  ##      while Peptides is nonempty
  ##          Peptides = Expand(Peptides)
  ##          for each peptide Peptide in Peptides
  ##              if Mass(Peptide) == ParentMass(Spectrum)
  ##                  if Cyclospectrum(Peptide) == Spectrum
  ##                      output Peptide
  ##                  remove Peptide from Peptides
  ##              else if Peptide is not consistent with Spectrum
  ##                  remove Peptide from Peptides

  imt <- integermasstable()
  nreps <- 0
  peptides <- rep("",1)
  while (length(peptides) > 0)
    {
      ## I believe expand (the branch step) is working correctly.
      peptides <- expand(peptides, imt$proteins)
      
      ## Now add the bound step
      for (i in 1:length(peptides)) {
        if(imt$intmass
      }
      
      ## Just added this here for now until I determine that I won't create an
      ## infinite loop using the loop condition (length(peptides) > 0)
      nreps <- nreps + 1
      if (nreps > 1000000)
        stop("Too many repetitions.  Exiting loop to prevent an infinite loop")
    }
}




expand <- function(peptides, aminoacids) {
  ## Add one of each amino acid to the existing peptide list (given by peptides).
  ## e.g. if peptides[i] = "AQ", then add "AQA", "AQC", "AQD", ..., "AQY"

  for (i in 1:length(peptides))
    {
      for (j in 1:length(aminoacids)) {
        peptides <- c(peptides, paste(peptides[i], aminoacids[j], sep=""))
        #print(peptides)
      }
    }
  ## I think I want only unique peptides...?
  return(unique(peptides))
}



runcyclicspectrum <- function(peptide, imt) {
  #imt <- integermasstable()
  peptide <- strsplit(peptide,"")[[1]]
  cyclicspec <- cyclicspectrum(peptide, imt$proteins, imt$intmass)
  
  return(cyclicspec)
}




cyclicspectrum <- function(peptide, aminoacid, aminoacidmass) {
  ## peptide is the peptide for which we are determining the sequence
  ## aminoacid is a vector of amino acids, and the aminoacidmass
  ## parameter is a vector of amino acid masses.  This is
  ## structured so that for all i in aminoacid, the mass of aminoacid[i]
  ## equals aminoacidmass[i].
  prefixmass <- rep(0,1)  
  
  for (i in 1:length(peptide)) {
    for (j in 1:20) {
      if(aminoacid[j] == peptide[i]) {
        if (i > 1)          
            prefixmass <- c(prefixmass, prefixmass[i] + aminoacidmass[j])
        else
           prefixmass <- c(prefixmass, aminoacidmass[j])
      }
    }
  }
  peptidemass <- prefixmass[length(prefixmass)]

  cyclspec <- rep(0,1)
  for (i in 1:length(peptide)) {
    for (j in (i + 1):(length(peptide) + 1)) {
      cyclspec <- c(cyclspec, prefixmass[j] - prefixmass[i])

      if (i > 1 && j < (length(peptide) + 1))
        cyclspec <- c(cyclspec, (peptidemass - (prefixmass[j] - prefixmass[i])))
    }
  }

  return(sort(cyclspec))
}




integermasstable <- function() {
  proteins <- c('G', 'A', 'S', 'P', 'V', 'T', 'C', 'I', 'L', 'N', 'D', 'K', 'Q', 'E', 'M', 'H', 'F', 'R', 'Y', 'W')
  intmass <- as.integer(c(57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186))

  df <- data.frame(cbind(proteins, intmass), stringsAsFactors=FALSE)
  df$intmass <- as.integer(df$intmass)

  return(df)
}
