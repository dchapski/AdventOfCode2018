# The goal is to determine the order of the steps
df <- read.table("Day_07_Input.txt", stringsAsFactors=F)
df <- (df[, c(2, 8)])

sort.order <- c()

# Find vertices with no incoming edge, sort alphabetically, take first
while(nrow(df) > 0) {
  if(nrow(df) == 1) {
    last.two <- as.vector(df[1, ])
    names(last.two) <- NULL
    sort.order <- c(sort.order, last.two) # add last two letters
    break
  }

  no.in.edge <- sort(unique(df[!(df[, 1] %in% df[, 2]), 1]))[1]

  # The other letters that would have been in no.in.edge are now still
  # available for incorporation into the sort (after this current iteration)
  sort.order <- c(sort.order, no.in.edge)
  df <- df[-which(df[, 1] == no.in.edge), ]
}

print(paste(sort.order, collapse="")) # Solution to Part 1

################################################################################

# Given the steps and times needed to complete them, determine the total time 
# needed to complete all the steps, with 5 workers

# This is an R version of an elegant solution presented in:
# www.reddit.com/r/adventofcode/comments/a3wmnl/2018_day_7_solutions/eb9sfnm

df <- read.table("Day_07_Input.txt", stringsAsFactors=F)
df <- (df[, c(2, 8)])

edges <- vector("list", length(LETTERS)) # edge list
names(edges) <- LETTERS
for(i in 1:nrow(df)) {
  x.letter <- df[i, 1]
  edges[[x.letter]] <- c(edges[[x.letter]], df[i, 2])
}

edges[sapply(edges, is.null)] <- NULL # get rid of null one
for(i in 1:length(edges)) {
  edges[[i]] <- sort(edges[[i]])
}

in.deg <- rep(0, length(LETTERS)) # in.degree named vector
names(in.deg) <- LETTERS
in.deg[names(table(df[, 2]))] <- table(df[, 2]) # replace with in degree

times <- order(LETTERS) + 60 # times required for given step
names(times) <- LETTERS

t <- 0 # timer
ev <- c() # events
q <- c() # queue 

add.task <- function(task) {
  q <<- c(q, task)
}

start.work <- function(task) {
  while(length(ev) < 5 & length(q) > 0) {
    x <- min(q)
    q <<- q[which(q != x)]
    ev <<- c(ev, t+times[x])
    message(paste0("Starting ", x, " at t = ", t))
  }
}

for(k in names(edges)) {
  if(in.deg[k] == 0) {
    add.task(k)
  }
}
start.work()

while(length(ev) > 0 | length(q) > 0) {
  t <- min(ev)
  x <- ev[which.min(ev)]
  print(x)
  ev <- ev[-which((names(ev) %in% names(x)))]
  for(y in edges[[names(x)]]) {
    in.deg[y] <- in.deg[y] - 1
    if(in.deg[y] == 0) add.task(y)
  }
  start.work()
}

print(t) # Solution to Part 2
