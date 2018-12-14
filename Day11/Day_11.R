options(scipen=999) # For multiplication and parsing of large numbers

# Puzzle input, also known as grid serial number
gsn <- scan("Day_11_Input.txt")

# Define function to take character x,y coords and return power calculation
getPower <- function(coords) {
  coords <- unlist(strsplit(coords, split=",", fixed=TRUE))
  x <- as.numeric(coords[1])
  y <- as.numeric(coords[2])
  rack.id <- x + 10
  rack.id.y <- rack.id * y
  power <- rack.id.y + gsn
  power <- power * rack.id
  if(nchar(power) >= 3) {
    power <- unlist(strsplit(as.character(power), split=""))
    power <- as.numeric(power[length(power) - 2])
  } else {
    power <- 0
  }
  power <- power - 5
  return(power)
}

mat <- t(outer(1:300, 1:300, FUN=paste, sep=",")) # Populate with coords
mat <- apply(mat, MARGIN=c(1, 2), FUN=getPower) # Use coords to calculate power

# Define function to get total power for every 3x3 grid in the matrix
# Remember the orientation of top-left and top-right from the problem
totalPower <- function(x, y, mat=mat) {
  # Reject regions that are not amenable to FULL 3x3 square creation
  if(x < 2 | x > ncol(mat) - 1) return(NA)
  if(y < 2 | y > nrow(mat) - 1) return(NA)
  
  # Get coords of the 3x3 window, and sum up the power scores
  x.coords <- (x - 1):(x + 1)
  y.coords <- (y - 1):(y + 1)
  total.power <- sum(mat[x.coords, y.coords])
  return(total.power)
} 

# How to vectorize a function in R:
# https://stackoverflow.com/a/7395543
totalPowerVec <- Vectorize(totalPower, vectorize.args=c("x", "y"))
total.powers <- outer(1:nrow(mat), 1:ncol(mat), totalPowerVec, mat=mat)

# Get the center coordinate of the 3x3 square with max total power score
coord <- which(total.powers == max(total.powers, na.rm=TRUE), arr.ind=TRUE)
coord.y <- coord[1]
coord.x <- coord[2]

# Top left
print(paste0(coord.x - 1, ",", coord.y - 1)) # Solution to Part 1

################################################################################

# Define function to get total power for every nxn grid in the matrix
# Remember the orientation of top-left and top-right from the problem
# Start from top-left corner
totalPower <- function(x, y, mat=mat, n) {
  # Reject regions that are not amenable to FULL 3x3 square creation
  if(x > ncol(mat) - n + 1) return(NA)
  if(y > nrow(mat) - n + 1) return(NA)
  
  # Get coords of the 3x3 window, and sum up the power scores
  x.coords <- x:(x + n - 1)
  y.coords <- y:(y + n - 1)
  total.power <- sum(mat[x.coords, y.coords])
  return(total.power)
}

# How to vectorize a function in R:
# https://stackoverflow.com/a/7395543
totalPowerVec <- Vectorize(totalPower, vectorize.args=c("x", "y"))

# Get the center coordinate of the nxn square with max total power score
top.max <- 0
names(top.max) <- "none"

for(i in 1:300) {
  total.powers <- outer(1:nrow(mat), 1:ncol(mat), totalPowerVec, mat=mat, n=i)
  current.max <- max(total.powers, na.rm=TRUE)
  if(current.max > top.max) {
    coord <- which(total.powers == current.max, arr.ind=TRUE)
    coord.y <- coord[1]
    coord.x <- coord[2]
    top.max <- current.max
    names(top.max) <- paste0(coord.x, ",", coord.y, ",", i)
  }
  if(i %% 25 == 0) message("Working on ", i)
}

print(top.max) # Solution to Part 2
