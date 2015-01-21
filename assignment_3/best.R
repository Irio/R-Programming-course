outcome_of_care_measures <- function(outcome, state = NULL) {
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
    names(data)[names(data) == outcome_column] <- "outcome"
    without_nas <- data[!is.na(data["outcome"]), ]
    
    if (is.null(state))
        without_nas
    else
        subset(without_nas, State == state)
}

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    data <- outcome_of_care_measures(outcome, state)
    
    if (nrow(data) == 0)
        stop("invalid state")
    
    sorted_hospitals <- data[order(data["outcome"], data$Hospital.Name), ]
    head(sorted_hospitals[, "Hospital.Name"], 1)
}