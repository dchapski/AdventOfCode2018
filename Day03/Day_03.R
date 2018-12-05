# Determine how many square inches of claims overlap on the fabric
cl <- read.table("Day_03_Input.txt", header=F, 
                 stringsAsFactors=F, comment.char="")

# The coordinates in column 3 define the width respect to the left, top
# It's not elegant, but this will get the job done
ustrsplit1 <- function(x) unlist(strsplit(x, split=",", fixed=T))[1]
cl$from.left <- sapply(X=cl$V3, FUN=ustrsplit1, USE.NAMES=F)
ustrsplit2 <- function(x) unlist(strsplit(x, split=",", fixed=T))[2]
cl$from.top <- sapply(X=cl$V3, FUN=ustrsplit2, USE.NAMES=F)
cl$from.top <- gsub(pattern=":", x=cl$from.top, replacement="", fixed=T)
ustrsplit3 <- function(x) unlist(strsplit(x, split="x", fixed=T))[1]
cl$wide <- sapply(X=cl$V4, FUN=ustrsplit3, USE.NAMES=F)
ustrsplit4 <- function(x) unlist(strsplit(x, split="x", fixed=T))[2]
cl$tall <- sapply(X=cl$V4, FUN=ustrsplit4, USE.NAMES=F)

cl$from.left <- as.numeric(cl$from.left)
cl$from.top <- as.numeric(cl$from.top)
cl$wide <- as.numeric(cl$wide)
cl$tall <- as.numeric(cl$tall)

# For each point on the 1000 x 1000 plane, determine how many rectangles overlap
# Left, right, top, bottom
cl$x1 <- cl$from.left + 1 # Need to be on the square
cl$x2 <- cl$x1 + cl$wide - 1 # Need to be on the last square
cl$y1 <- 1001 - ((cl$from.top + 1) + cl$tall) # Lowest part of box
cl$y2 <- cl$y1 + cl$tall - 1 # Need to be on the last square

# Define a function to determine, for a coordinate, how many rectangles overlap
fillMat <- function(coord.vec) {
  super.mat <- matrix(0, nrow=1000, ncol=1000)
  for(i in 1:nrow(coord.vec)) {
    x1 <- coord.vec[i, "x1"]
    x2 <- coord.vec[i, "x2"]
    y1 <- coord.vec[i, "y1"]
    y2 <- coord.vec[i, "y2"]
    super.mat[x1:x2, y1:y2] <- super.mat[x1:x2, y1:y2] + 1
  } # end i loop
  return(super.mat)
}

filled.mat <- fillMat(coord.vec=as.matrix(cl[, 9:12]))
print(length(which(filled.mat > 1))) # Solution to Part 1

################################################################################

# Determine claim that does not overlap with anything else
claimCheck <- function(coord.vec, sum.mat) {
  for(i in 1:nrow(coord.vec)) {
    x1 <- coord.vec[i, "x1"]
    x2 <- coord.vec[i, "x2"]
    y1 <- coord.vec[i, "y1"]
    y2 <- coord.vec[i, "y2"]
    mat.coords <- sum.mat[x1:x2, y1:y2]
    
    if(all(mat.coords == 1)) return(i)
    
    # if(sum(mat.coords) == (length(x1:x2) * length(y1:y2))) return(i)
  } # end i loop
}

good.ID <- claimCheck(coord.vec=as.matrix(cl[, 9:12]), sum.mat=filled.mat)
print(good.ID) # Solution to Part 2
