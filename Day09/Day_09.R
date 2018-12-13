# The goal of this script is to determine the winning Elf's score, given an 
# input of x players and a last marble value of y
# The container package is necessary for this script to run properly.
library(container)
p <- scan("Day_09_Input.txt", what="character")
num.players <- as.numeric(p[1])
high <- as.numeric(p[length(p) - 1])

# Set up a score list that includes each player
scores <- rep(0, num.players)
names(scores) <- 1:num.players

circle <- deque(0)

getMax <- function(i) {
  if(i %% 23 == 0) {
    # Determine the current player based on current marble and number of 
    # players in the game. Modulo will return a remainder, which is the 
    # number of the player.
    current.player <- i %% num.players
    
    # Current player keeps current marble for score of i
    # Current player also keeps marble that's 7 marbles counter-clockwise, 
    # and new current marble is now the one directly to the right
    invisible(rotate(x=circle, n=7))
    # Will be: current.score plus last value (now gone)
    scores[current.player] <<- scores[current.player] + i + invisible(pop(circle))
    invisible(rotate(x=circle, n=-1))
  } else {
    # Add current marble to the circle at the penultimate position, 
    # by first rotating it to get rid of the last position, and 
    # inserting the new marble score at the last position. Note that 
    # it all stays a circle.
    invisible(rotate(x=circle, n=-1)) #  Clockwise by 1, so last entry goes to beginning
    invisible(add(circle, i)) # Add to the right side, this is current marble
  }
  return(NULL)
}

temp <- sapply(1:high, FUN=getMax, USE.NAMES=F)
print(max(scores, na.rm=TRUE)) # Solution to Part 1

################################################################################

scores <- rep(0, num.players)
names(scores) <- 1:num.players

circle <- deque(0)

temp <- sapply(1:(high*100), FUN=getMax, USE.NAMES=F)
print(max(scores, na.rm=TRUE)) # Solution to Part 2

