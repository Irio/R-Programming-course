# setwd("/Users/irio/Code/r_programming/assignment_3")

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    outcome_column <- if (outcome == "heart attack")
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    else if (outcome == "heart failure")
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    else if (outcome == "pneumonia")
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    else
        stop("invalid outcome")
    
    data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    data[, outcome_column] <- as.numeric(data[, outcome_column])
    wo_nas <- data[!is.na(data[outcome_column]), ]
    from_state <- subset(wo_nas, State == state)
    
    if (nrow(from_state) == 0)
        stop("invalid state")
    
    ordered_hospitals <- from_state[order(from_state[outcome_column], from_state$Hospital.Name), ]
    head(ordered_hospitals[, "Hospital.Name"], 1)
}