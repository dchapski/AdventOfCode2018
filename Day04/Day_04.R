# Part 1 asks to determine the guard most likely to fall asleep the most

# Get the dates in Date class and sort so we can do a chronological analysis
df <- read.table("Day_04_Input.txt", 
                 header=F, sep="]", stringsAsFactors=F, comment.char="")

# Parse first column to get date
df[, 1] <- gsub(pattern="[", x=df[, 1], replacement="", fixed=T)
df$Date <- strptime(df[, 1], format="%Y-%m-%d %H:%M")

# Sort date
df <- df[order(df$Date), ]
row.names(df) <- NULL

# Define the minute of the hour
for(i in 1:nrow(df)) {
  df$Minute[i] <- as.numeric(unlist(strsplit(df[i, 1], split=":", fixed=T))[2])
}

# Determine sleep times
df$Diff <- NA # Amount of time asleep
for(i in 1:nrow(df)) {
  falls.asleep <- grepl(pattern="asleep", x=df[i, 2], fixed=T)
  if(falls.asleep) {
    df$Diff[i] <- difftime(df[i+1, "Date"], df[i, "Date"], units="mins")
  }
}

guard.list <- c()
sleep.times <- c()
last.one <- F

for(i in 1:nrow(df)) {
  current.guard <- df[i, 2]
  if(!grepl(pattern="begins", x=current.guard, fixed=T)) next
  
  # Get current shift guard ID
  current.guard <- unlist(strsplit(current.guard, split="#", fixed=T))[2]
  current.guard <- unlist(strsplit(current.guard, split=" ", fixed=T))[1]
  current.guard <- as.numeric(current.guard)
  
  # Add current guard to guard.list if necessary
  # Also add a spot in the sleep.times vector
  if(!(current.guard %in% guard.list)) {
    guard.list <- c(guard.list, current.guard)
    sleep.times <- c(sleep.times, 0) # Placeholder
  }
  
  # Determine next guard index
  next.guard.ind <- which(grepl(pattern="begins", 
                                x=df[(i+1):nrow(df), 2], fixed=T))[1]+i
  if(is.na(next.guard.ind)) {
    next.guard.ind <- nrow(df)
    last.one <- T
  }

  # Count sleep time for current shift
  sleep.vector <- as.numeric(df[i:(next.guard.ind-1), "Diff"])
  sleep.vector <- sum(sleep.vector, na.rm=T)
  sleep.vector <- sleep.vector + sleep.times[which(guard.list == current.guard)]
  sleep.times[which(guard.list == current.guard)] <- sleep.vector
  if(last.one) i <- next.guard
}

# Figure out which guard sleeps the most based on above analysis
which.guard <- which(sleep.times == max(sleep.times))
chosen.guard <- guard.list[which.guard]

# Determine which minute the chosen guard sleeps the most
# Set up a vector of minutes from 00 to 59

min.vec <- 0:59
min.sum.vec <- rep(0, length(min.vec))

for(i in 1:nrow(df)) {
  current.guard <- df[i, 2]
  if(!grepl(pattern="begins", x=current.guard, fixed=T)) next
  if(!grepl(pattern=chosen.guard, x=current.guard, fixed=T)) next

  # Determine next guard index
  next.guard.ind <- which(grepl(pattern="begins", 
                                x=df[(i+1):nrow(df), 2], fixed=T))[1]+i
  if(is.na(next.guard.ind)) {
    next.guard.ind <- nrow(df)
    last.one <- T
  }

  # Count sleep time for current shift
  sleep.vector <- as.numeric(df[i:(next.guard.ind-1), "Diff"])
  sleep.inds <- which(!is.na(sleep.vector)) + i - 1
  for(j in 1:length(sleep.inds)) {
    minutes <- df[sleep.inds[j]:(sleep.inds[j]+1), "Minute"]
    minutes <- minutes[1]:minutes[2]
    minutes <- minutes[-length(minutes)]
    min.vec.ind <- which(min.vec %in% minutes)
    min.sum.vec[min.vec.ind] <- min.sum.vec[min.vec.ind] + 1
  }
  
  if(last.one) i <- next.guard # end the loop
}

which.min <- which(min.sum.vec == max(min.sum.vec))
chosen.minute <- min.vec[which.min]

chosen.product <- chosen.guard * chosen.minute
print(chosen.product) # Solution to Part 1

################################################################################

# Now, determine which guard is most asleep on the same minute
min.vec <- 0:59
min.mat <- matrix(0, nrow=length(min.vec), ncol=length(guard.list))
row.names(min.mat) <- min.vec
colnames(min.mat) <- guard.list

for(i in 1:nrow(df)) {
  current.guard <- df[i, 2]
  if(!grepl(pattern="begins", x=current.guard, fixed=T)) next
  
  # Get current shift guard ID
  current.guard <- unlist(strsplit(current.guard, split="#", fixed=T))[2]
  current.guard <- unlist(strsplit(current.guard, split=" ", fixed=T))[1]
  current.guard <- as.numeric(current.guard)
  
  # Will use this index to populate data.frame
  guard.ind <- which(colnames(min.mat) == as.character(current.guard))
  
  # Determine next guard index
  next.guard.ind <- which(grepl(pattern="begins", 
                                x=df[(i+1):nrow(df), 2], fixed=T))[1]+i
  if(is.na(next.guard.ind)) {
    next.guard.ind <- nrow(df)
    last.one <- T
  }
  
  # Count sleep time for current shift
  sleep.vector <- as.numeric(df[i:(next.guard.ind-1), "Diff"])
  if(length(sleep.vector) == 1 && is.na(sleep.vector)) next
  sleep.inds <- which(!is.na(sleep.vector)) + i - 1
  for(j in 1:length(sleep.inds)) {
    minutes <- df[sleep.inds[j]:(sleep.inds[j]+1), "Minute"]
    minutes <- minutes[1]:minutes[2]
    minutes <- minutes[-length(minutes)]
    min.ind <- which(row.names(min.mat) %in% minutes)
    min.mat[min.ind, guard.ind] <- min.mat[min.ind, guard.ind] + 1
  }
  
  if(last.one) i <- next.guard # end the loop
}

# Determine the minute most slept, as well as the guard
minute.ind <- which(min.mat == max(min.mat), arr.ind=T)
minute.most.slept <- row.names(min.mat)[minute.ind[1]]
minute.most.slept <- as.numeric(minute.most.slept)
guard.most.slept <- guard.list[minute.ind[2]]

print(minute.most.slept * guard.most.slept) # Solution to Part 2

