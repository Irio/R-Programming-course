rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
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
    
    ordered_hospitals <- wo_nas[order(wo_nas$State, wo_nas[outcome_column], wo_nas$Hospital.Name), ]
    index <- if (num == "best")
        1
    else if (num == "worst") # ??????????????
        #nrow(ordered_hospitals)
        #max(tapply(wo_nas$Hospital.Name, ordered_hospitals$State, length))
    else
        num
    
    f <- ordered_hospitals[, c(outcome_column, "Hospital.Name", "State")]
    g <- aggregate(f, list(State = f$State), FUN = function(x) x[index])[, c("Hospital.Name", "State")]
    names(g) <- c("hospital", "state")
    g
}