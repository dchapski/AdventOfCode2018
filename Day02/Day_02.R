# For the input, calculate how many entries have two of any letter. Also 
# calculate how many entries have three of any letter. If an entry contains 
# two of any letter more than once, it only counts the first occasion. Same for 
# three letter instances.
ids <- read.table("Day_02_Input.txt", header=F, stringsAsFactors=F)
colnames(ids) <- "ID"

# Define a function that takes an input and returns a logical for whether there 
# exists two or three of a letter
anySpecificRepeats <- function(id, num.repeats) {
  
  # Split the string into individual letters
  id.split <- unlist(strsplit(id, split="", fixed=T))
  
  # Summary will provide the count for each letter in string
  id.summary <- summary(factor(id.split))
  
  # If summary has count of num.repeats, return TRUE
  if(num.repeats %in% id.summary) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

# Populate two new columns with logical vector
ids$two.letters <- sapply(X=ids$ID, 
                          FUN=anySpecificRepeats, num.repeats=2, USE.NAMES=F)
ids$three.letters <- sapply(X=ids$ID, 
                            FUN=anySpecificRepeats, num.repeats=3, USE.NAMES=F)

# Calculate sum of each column and multiply to generate checksum
# Note: The sum of a logical vector is the number of TRUE values
two.total <- sum(ids$two.letters)
three.total <- sum(ids$three.letters)
checksum <- two.total * three.total
print(checksum)

################################################################################

# Now for Part 2
# Find the two IDs that differ by exactly 1 letter. The answer to the problem 
# will be the letters that are in common.

# Split all strings
master.split <- sapply(X=ids$ID, FUN=strsplit, split="", fixed=T)

# Define a function to check for matches
singleMismatch <- function(x, y) {
  if(length(x) != length(y)) break
  log.vec <- x == y
  if(sum(!log.vec) == 1) print(paste(x[log.vec], collapse=""))
}

# Run an i,j nested loop and run the function within
for(i in 1:length(master.split)) {
  current.hot <- master.split[[i]]
  for(j in 1:length(master.split)) {
    if(i == j | i > j) next
    current.candidate <- master.split[[j]]
    singleMismatch(x=current.hot, y=current.candidate)
  } # end j loop
} # end i loop

