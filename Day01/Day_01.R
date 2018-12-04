# Read text input and calculate sum of frequencies, starting from 0
freqs <- read.table("Day_01_Input.txt", header=F, stringsAsFactors=F)

# Coerce to vector
freqs <- as.vector(freqs[, 1])

# This is the answer to Part 1 (a sum of all the frequencies)
print(sum(freqs))

# Now, the challenge for Part 2 is to determine the first current frequency that
# is reached TWICE. The list can loop more than once
current.freq <- 0
all.freqs <- 0

repeat {
  for(i in 1:length(freqs)) {
    current.freq <- current.freq + freqs[i]
    all.freqs <- c(all.freqs, current.freq)
    current.dup <- duplicated(all.freqs)
    if(sum(current.dup) > 0) {
      return(print(all.freqs[which(current.dup)])) # The answer to Part 2
    }
  }
}
