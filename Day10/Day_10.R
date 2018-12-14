library(ggplot2)

df <- read.table("Day_10_Input.txt", sep="=", stringsAsFactors=FALSE)

getCoords <- function(x) {
  char.coords <- gsub(pattern="<|>| |velocity", x=x, replacement="")  
  return(char.coords)
}

# Get the X coordinates from the "x,y" coords from getCoords
getX <- function(coords) {
  coords <- unlist(strsplit(coords, split=",", fixed=TRUE))[1]
  coords <- as.numeric(coords)
  return(coords)
}

# Remember to multiply the Y coordinates by -1, for both pos and vel
getY <- function(coords) {
  coords <- unlist(strsplit(coords, split=",", fixed=TRUE))[2]
  coords <- -as.numeric(coords)
  return(coords)
}

pos <- sapply(df[, 2], FUN=getCoords, USE.NAMES=FALSE)
vel <- sapply(df[, 3], FUN=getCoords, USE.NAMES=FALSE)

master <- data.frame(pos.x=sapply(pos, FUN=getX, USE.NAMES=FALSE),
                     pos.y=sapply(pos, FUN=getY, USE.NAMES=FALSE),
                     vel.x=sapply(vel, FUN=getX, USE.NAMES=FALSE),
                     vel.y=sapply(vel, FUN=getY, USE.NAMES=FALSE))
master <- as.matrix(master)

# Define a function to move the coordinates by the velocity
moveCoords <- function(coords, multiplier=1) {
  coords[1] <- coords[1] + (coords[3] * multiplier)
  coords[2] <- coords[2] + (coords[4] * multiplier)
  return(coords)
}

# Have scroll through this part by hand. Solutions to Part1 and Part2
pdf("Day_10_Ouptut.pdf")
for(i in 10700:10850) {
  current <- apply(X=master, MARGIN=1, FUN=moveCoords, multiplier=i)
  current <- as.data.frame(t(current))
  if(i %% 1 == 0) {
    p <- ggplot(data=current, aes(x=pos.x, y=pos.y)) +
      geom_point(size=1, shape=15) + theme_bw() +
      coord_cartesian(xlim=c(0, 280), ylim=c(-125, -250)) + 
      ggtitle(paste0("Time: ", i))
    print(p)
  }
}
dev.off()
