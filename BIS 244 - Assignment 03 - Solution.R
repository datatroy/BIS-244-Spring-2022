# Solution file for BIS 244 Assignment 03, Spring 2022
rm(list=ls(all=TRUE))
dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()
cat("\014")


library(here)
library(tidyverse)

# Reading the data and setting up working data frame
votes <- read_csv(here("Data","PRESIDENT_precinct_general.zip"))
n_votes <- length(votes$office)
states <- distinct(votes,state,.keep_all=TRUE)
states <- subset(states,select=state)
states$Biden <- 0
states$Trump <- 0

# Getting ready for for() loops to summarize from votes into states
n_states <- length(states$state)

COUNTER <- 0
# Let's start a timer....
ptm <- proc.time()
for (i in 1:n_votes) {
  for (j in 1:n_states) {
    if (votes$state[i] == states$state[j]) {
      if (votes$candidate[i] == "JOSEPH R BIDEN") {
        states$Biden[j] <- states$Biden[j] + votes$votes[i] 
      }
      if (votes$candidate[i] == "DONALD J TRUMP") {
        states$Trump[j] <- states$Trump[j] + votes$votes[i] 
      }
      j <- n_states
    }
  }
  # Following is just to give "visual feedback"  
  if (round(i/n_votes*100, digits = 0) > COUNTER) {
    COUNTER <- round(i/n_votes*100, digits = 0)
    cat(COUNTER,"pct complete\n")}
}

# Stop the timer
TIMER <- (proc.time() - ptm)
TIMER


# A much quicker way

temp <- votes[which(votes$candidate=="JOSEPH R BIDEN" |
                       votes$candidate=="DONALD J TRUMP"),]

temp <- aggregate(votes ~ state + candidate, data = temp, FUN = "sum")

names(temp)

BIDEN <- temp[which(temp$candidate=="JOSEPH R BIDEN"),]
names(BIDEN)[3]<-"BIDEN"
BIDEN <- select(BIDEN,state,BIDEN)

TRUMP <- temp[which(temp$candidate=="DONALD J TRUMP"),]
names(TRUMP)[3]<-"TRUMP"
TRUMP <- select(TRUMP,state,TRUMP)

QUICKER <- merge(BIDEN,TRUMP, by.x="state",by.y="state")

QUICKER


# Converting this to data that's easy to graph

states$Winner <- ""
states$Winner_Votes <- 0

for (i in 1:n_states) {
  if (states$Biden[i] > states$Trump[i]) {
    states$Winner[i] <- "Biden"
    states$Winner_Votes[i] <- states$Biden[i]
  } else {
    states$Winner[i] <- "Trump"
    states$Winner_Votes[i] <- states$Trump[i]
  }
}

# First type of plot that most of you probably did
p <- ggplot(data = states,
            mapping = aes(x = state,
                          y = Winner_Votes,
                          color = Winner))

p + geom_point()

# "Flipped" graph that's at least readable

p <- ggplot(data = states,
            mapping = aes(y = state,
                          x = Winner_Votes,
                          color = Winner))

p + geom_point()

# And (which I didn't require) correcting the colors as per video set 9

library(ggrepel)
library(RColorBrewer)

party_colors <- c("#2E74C0", "#CB454A")

p <- ggplot(data = states,
            mapping = aes(y = state,
                          x = Winner_Votes,
                          color = Winner))

p + geom_point()+ scale_color_manual(values = party_colors)


