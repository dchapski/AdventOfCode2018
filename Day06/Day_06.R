# Determine the coordinate with the largest area of closest points in space

# Read in coordinates (50 of them)
df <- as.matrix(read.table("Day_06_Input.txt", sep=",", stringsAsFactors=F))
rownames(df) <- paste0(df[, 1], ", ", df[, 2])
length(unique(row.names(df))) # They're all unique

# Determine the edges of the space
outer.x <- max(df[, 1])
outer.y <- max(df[, 2])
inner.x <- min(df[, 1])
inner.y <- min(df[, 2])
range.x <- inner.x:outer.x
range.y <- inner.y:outer.y

# Make each coord in the empty mat the coords themselves
getPos <- function(x,y){
  paste0(x + inner.x - 1, ", ", y + inner.y - 1) # Need to get into right part of space
}

# Vectorize the function
getPosVectorized <- Vectorize(getPos,vectorize.args = c('x','y'))

# Generate a matrix with the coordinates in each entry
# https://stackoverflow.com/questions/7395397/how-to-apply-function-over-each-matrix-elements-indices
o.m <- outer(1:length(range.x), 1:length(range.y), getPosVectorized)

# Keep going to find closest point to each other point
getClosest <- function(coords, comparison.df=df) {
  new.coords <- as.numeric(unlist(strsplit(coords, split=", ", fixed=T)))
  current.dist <- rbind(c(new.coords[1], new.coords[2]), comparison.df)
  # Will be length 50
  current.dist <- as.matrix(dist(current.dist, method="manhattan"))[-1, 1]
  current.min <- which.min(current.dist)
  if(sum(current.min %in% current.dist) > 1) return(NA)
  current.dist <- which.min(current.dist)
  current.dist <- names(current.dist)
  return(current.dist)
}

new.mat <- apply(o.m, MARGIN=c(1, 2), FUN=getClosest)

# Scan edges to make anything that's touching them NA
edges <- unique(c(o.m[1, ], o.m[nrow(o.m), ], 
                  o.m[, 1], o.m[, ncol(o.m)]))

# Coords without infinite areas
good.coords <- summary(factor(new.mat[!(new.mat %in% edges)]))

# Value of largest area (not Inf)
print(good.coords[which.max(good.coords)]) # Solution to Part 1

################################################################################

# Part 2 asks to get the sum of distances from each cordinate to all others
getDistances <- function(coords, comparison.df=df) {
  new.coords <- as.numeric(unlist(strsplit(coords, split=", ", fixed=T)))
  current.dist <- rbind(c(new.coords[1], new.coords[2]), comparison.df)
  current.dist <- as.matrix(dist(current.dist, method="manhattan"))[-1, 1]
  current.sum <- sum(current.dist)
  if(current.sum < 10000) return("good") # 10000 is hard coded per the puzzle
  return("bad")
}

new.mat <- apply(o.m, MARGIN=c(1, 2), FUN=getDistances)

# Get number of coordinates with distance sum < 10000
print(sum(new.mat == "good")) # Solution to Part 2
