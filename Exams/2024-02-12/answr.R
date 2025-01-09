


# Question 1
library(ape)
getwd()
fast <- read.FASTA('C:/Users/johan/Desktop/Plugg/Statistik/Bioinformatik/Bio/Exams/2024-02-12/S_protein.fasta')

print(fast)

trans(fast[1])

# Question 2

seq_1 <- "GGCGT"
seq_2 <- "GGTCCT"

# Load the stringdist package
library(stringdist)
# Calculate Levenshtein distance
lev_distance <- stringdist(seq_1, seq_2, method = "lv")
print(lev_distance)

# global alignment on paper.


needleman_wunsch <- function(seq1, seq2, match = 2, mismatch = -1, gap = -1) {
  # Initialize the dimensions of the dynamic programming table
  m <- nchar(seq1)
  n <- nchar(seq2)
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Fill the first row and column with gap penalties
  for (i in 1:(m + 1)) dp[i, 1] <- (i - 1) * gap
  for (j in 1:(n + 1)) dp[1, j] <- (j - 1) * gap
  
  # Convert sequences to character vectors
  seq1 <- unlist(strsplit(seq1, ""))
  seq2 <- unlist(strsplit(seq2, ""))
  
  # Fill the DP table
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      match_mismatch <- ifelse(seq1[i - 1] == seq2[j - 1], match, mismatch)
      dp[i, j] <- max(
        dp[i - 1, j - 1] + match_mismatch, # Match/Mismatch
        dp[i - 1, j] + gap,               # Gap in seq2
        dp[i, j - 1] + gap                # Gap in seq1
      )
    }
  }
  
  # Return the dynamic programming table
  return(dp)
}

dp_table <- needleman_wunsch(seq_1, seq_2, match = 2, mismatch = -1, gap = -1)
print(dp_table)

library(SeqAlignR)

?SeqAlignR
# Run the Needleman-Wunsch algorithm
align_sequences(seq_1, seq_2, d = -1, mismatch = -1, match = 2, method="needleman")
plot(align_sequences(seq_1, seq_2, d = -1, mismatch = -1, match = 2, method="needleman"))
#
"It's a way of arranging sequences(DNA,RNA, Protein) to identify similarities, that can be functiona, structional or evolutionary"




# Question 3

# on paper:

# tree nr 1 and 3 has 2 changes, nr 2 has 1 change.

