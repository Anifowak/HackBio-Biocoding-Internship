# Define the function to translate DNA to protein
translate_dna_to_protein <- function(dna_sequence) {
                # Convert DNA to RNA
                rna_sequence <- gsub("T", "U", toupper(dna_sequence))
                
                # Define the genetic code
                genetic_code <- c(
                                UUU = "F", UUC = "F", UUA = "L", UUG = "L",
                                CUU = "L", CUC = "L", CUA = "L", CUG = "L",
                                AUU = "I", AUC = "I", AUA = "I", AUG = "M",
                                GUU = "V", GUC = "V", GUA = "V", GUG = "V",
                                UCU = "S", UCC = "S", UCA = "S", UCG = "S",
                                CCU = "P", CCC = "P", CCA = "P", CCG = "P",
                                ACU = "T", ACC = "T", ACA = "T", ACG = "T",
                                GCU = "A", GCC = "A", GCA = "A", GCG = "A",
                                UAU = "Y", UAC = "Y", UAA = "Stop", UAG = "Stop",
                                CAU = "H", CAC = "H", CAA = "Q", CAG = "Q",
                                AAU = "N", AAC = "N", AAA = "K", AAG = "K",
                                GAU = "D", GAC = "D", GAA = "E", GAG = "E",
                                UGU = "C", UGC = "C", UGA = "Stop", UGG = "W",
                                CGU = "R", CGC = "R", CGA = "R", CGG = "R",
                                AGU = "S", AGC = "S", AGA = "R", AGG = "R",
                                GGU = "G", GGC = "G", GGA = "G", GGG = "G"
                )
                
                # Split RNA sequence into codons
                codons <- substring(rna_sequence, seq(1, nchar(rna_sequence) - 2, 3), seq(3, nchar(rna_sequence), 3))
                
                # Translate codons to amino acids
                protein <- sapply(codons, function(codon) genetic_code[[codon]])
                
                # Combine amino acids into a protein string
                protein_sequence <- paste(protein, collapse = "")
                
                # Return the protein sequence
                return(protein_sequence)
}


#Translate AGCTGTAAGCTGACCTGGAATCGT to protein
dna_sequence <- "AGCTGTAAGCTGACCTGGAATCGT"
protein_sequence <- translate_dna_to_protein(dna_sequence)

#display the protein sequence
print(protein_sequence)





