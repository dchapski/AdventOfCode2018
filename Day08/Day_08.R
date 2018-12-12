# Load the input and determine the nodes
# A node always consists of a header (2 numbers)
#   Quantity of child nodes
#   Quantity of metadata entries
# A node also contains 0 or more child nodes (specified in header)
# A node also contains 0 or more metadata entries (specified in header)

df <- scan("Day_08_Input.txt")

# Define a function to look at the first two entries and determine how many 
# child nodes and meta data entries there are
getStatus <- function(string, count=0) {
  value <- list()
  mysum <- 0
  num.child <- string[1]
  num.metadata <- string[2]
  string <- string[-c(1:2)]
  
  # If no child node, get metadata sum, else recursively go through child nodes
  if(num.child == 0) {
    metadata <- string[1:num.metadata]
    metadata.sum <- sum(metadata)
    count <- count + metadata.sum
    string <- string[-c(1:num.metadata)]
    return(list(string=string, count=count, value=metadata.sum))
  } else { 
    for(i in 1:num.child) { # This will go through subsequent child nodes
      current <- getStatus(string=string)
      string <- current$string # Update the string to work on
      value[length(value) + 1] <- current$value # Update the value of node
      count <- count + current$count # Update the count
    }
  }
  
  metadata <- string[1:num.metadata]
  metadata.sum <- sum(metadata)
  mysum <- sum(unlist(value[metadata]))
  count <- count + metadata.sum
  string <- string[-c(1:num.metadata)]
  return(list(string=string, count=count, value=mysum))
}

solution <- getStatus(string=df)
print(solution$count) # Solution to Part 1
print(solution$value) # Solution to Part 2

