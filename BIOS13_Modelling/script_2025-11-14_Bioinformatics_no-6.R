## Clear all variables
rm(list=ls())

## Load mitochondrial sequence data
mito_fasta <- read.fasta('datasets/NC_012920.fasta')
mito_seq <- mito_fasta$NC_012920.1
mito_seq <- as.vector(mito_seq)

## Print length of sequence
print(length(mito_seq))

## Call nucleo_content (function from N°4) to print the proportions of 
## the G, C, A and T nucelotides
nucleo_content <- function(seqv) {
  # Print out the relative frequencies of all nucleotids
  Gsum <- sum(seqv=='g')
  Csum <- sum(seqv=='c')
  Asum <- sum(seqv=='a')
  Tsum <- sum(seqv=='t')
  cat('G : ',Gsum/length(seqv)*100,'%\n',sep='')
  cat('C : ',Csum/length(seqv)*100,'%\n',sep='')
  cat('A : ',Asum/length(seqv)*100,'%\n',sep='')
  cat('T : ',Tsum/length(seqv)*100,'%\n',sep='')
  # no return value
}
nucleo_content(mito_seq)

## Calls create_complement (function from N°5) to create the complementary 
## sequence and then nucleo_content to print its nucelotide proportions
create_complement <- function(seq){
  reverse <- 0
  for(i in length(seq):1){ ## use x from function(x) instead of vectorname
    reverse[-i+length(seq)+1] <- seq[i]
  }
  return(reverse)
  print(reverse)
}
mito_seq_rev <- create_complement(mito_seq)

nucleo_content(mito_seq_rev)
