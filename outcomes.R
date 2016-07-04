## ------------------------------------------------------
##      Programming Assignment 3 - Health Care
## ------------------------------------------------------

setwd("~/git/Coursera/ProgAss3")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

headings <- outcome[0,]
str(headings)

best <- function(state, outcome) {
    # Read outcome data
    d <- read.csv("outcome-of-care-measures.csv")
    
    # Check arguments are valid
    
    # Return Hospital name in that state with lowest 30-day death rate
}