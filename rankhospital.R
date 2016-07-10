## ------------------------------------------------------
##      Programming Assignment 3 - Health Care
## ------------------------------------------------------

setwd("C:/Users/bcrosby/git/Coursera/ProgAss3")


rankhospital <- function(state, outcome, num = "best") {
    # Error Checking - valid outcome and valid rank
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
    if(!num %in% c("best", "worst") && !is.numeric(num)) stop("invalid ranking")
    
    # Set up indices for each condition. This matches the column numbers
    # in the data file but means I can refer to the data by a better name.
    Hospital     <- 2
    HeartAttack  <- 11
    HeartFailure <- 17
    Pneumonia    <- 23

    # Use a switch statement to choose the correct outcome
    Result <- switch (outcome,
                      "heart attack"  = HeartAttack,
                      "heart failure" = HeartFailure,
                      "pneumonia"     = Pneumonia)

    # I want to only read in the columns that I'm going to use
    # Use the indexes defined above to create a column import list
    TotalCols <- 46               # the total number of columns in the data file
    SkipCols <- (Result - 1) - 7  # The # of columns between the state and the outcome
    EndCols <- TotalCols - Result # the # of columns after the outcome
    ColTypes <- c("NULL",
                  "character",
                  rep("NULL", 4), # pad out between column 2 and 7
                  "factor",
                  rep("NULL", SkipCols), # skip the columns between state and outcome
                  NA,
                  rep("NULL", EndCols)) # skip unwanted columns at the end of the file

    # Read outcome-of-care-measures.csv
    OutcomeTable <- read.csv("outcome-of-care-measures.csv",
                             header = TRUE,
                             na.strings = "Not Available",
                             stringsAsFactors = FALSE,
                             colClasses = ColTypes)
    
    # Rename the columns to make them easier to work with
    colnames(OutcomeTable) <- c("Hospital", "State", "Result")

    # Now that we've read the data, confirm the State is valid
    if(! state %in% OutcomeTable$State) stop("invalid state")
    
    # Remove any NA values
    OutcomeTable <- na.omit(OutcomeTable[OutcomeTable$State == state, ])

    # # Order the results by outcome Result and then Hospital name
    OutcomeTable <- OutcomeTable[order(OutcomeTable$Result, OutcomeTable$Hospital), ]
    
    # if the input value was either "best" or "worst" convert that to an index value
    rank <- switch(num,
                   "best" = 1,
                   "worst" = nrow(OutcomeTable))
    
    # If rank is not set then use the numeric value passed in
    if(!is.numeric(rank)) rank <- as.numeric(num)
    
    # Return the value
    OutcomeTable[rank, "Hospital"]
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MD", "heart attack", "best")
rankhospital("MN", "heart attack", 5000)
