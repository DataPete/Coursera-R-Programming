## This function finds the best hospital in a state by finding the hospital
## with the lowest 30-day mortality rate for the specified outcome in that state

## Outcomes specified are "heart attack" [11], "heart failure" [17], 
## "pneumonia" [23]
## Hospitals that have "Not Available" or "NA" will be excluded 

best <- function(state, outcome) {
        ## Read outcome data
        d_file <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character",
                           na.strings = c("Not Available", "NA"))
        
        ## Check that state and outcome are valid
        states <- d_file[, 7]
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if ((state %in% states) == FALSE) {
                stop(print("invalid state"))
        }
        if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }
        
        ## Setting the column to match the outcome entered
        col_found <- if (outcome == "heart attack") {
                d_file[, 11] <- as.numeric(d_file[, 11])
                11
        }
        else if (outcome == "heart failure") {
                d_file[, 17] <- as.numeric(d_file[, 17])
                17
        }
        else {
                d_file[, 23] <- as.numeric(d_file[, 23])
                23
        }
        
        ##subsetting the data with the entered state
        subset_state <- subset(d_file, State == state)
        
        ## Return hospital name in that state with lowest 30-day death rate
        minValue <- min(subset_state[col_found], na.rm = T)
        minRow <- subset(subset_state, subset_state[[col_found]] == minValue)
        minRow$Hospital.Name
}                                                      