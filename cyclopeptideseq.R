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

  peptides <- ""
  while (peptides)
    {
      
    }
}




expand <- function(peptides) {
  ## Add one of each amino acid to the existing peptide list (given by peptides).
  ## e.g. if peptides = "AQ", then add "AQA", "AQC", "AQD", ..., "AQY"

}
