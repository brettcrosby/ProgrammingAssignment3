## ------------------------------------------------------
##      Programming Assignment 3 - Health Care
## ------------------------------------------------------

setwd("C:/Users/bcrosby/git/Coursera/ProgAss3")


rankall <- function(outcome, num = "best") {
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
    SkipCols <- (Result - 1) - 7  # The # of colums between the state and the outcome
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
    
    # Order the results by outcome Result and then Hospital name
    # This gives me the data ordered and ready to split by state
    OutcomeTable <- OutcomeTable[order(OutcomeTable$State, OutcomeTable$Result, OutcomeTable$Hospital), ]
    
    # Get rid of any NA values
    OutcomeTable <- na.omit(OutcomeTable)
    
    # Split the data frame by State
    split_data <- split(OutcomeTable, OutcomeTable$State)   
    
    # Use sapply to iterate through the list of state data frames and extract the
    # Hospital at the appropriate index
    Hospitals <- sapply(split_data, function(x) {
        if(num == "best") {
            xRow <- 1
        } else if(num == "worst") {
            xRow <- nrow(x)
        } else {
            xRow <-num
        }
        x[xRow, "Hospital"]
    })
    
    # Build a dataframe with the Hospital and State data
    FinalResult <- data.frame(hospital=Hospitals, state=names(Hospitals), row.names=names(Hospitals))

    # Return the value
    FinalResult
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

