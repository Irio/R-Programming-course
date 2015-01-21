source("best.R")

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    data <- outcome_of_care_measures(outcome, state)
    
    if (nrow(data) == 0)
        stop("invalid state")
    
    sorted_hospitals <- data[order(data["outcome"], data$Hospital.Name), ]
    index <- if (num == "best")
        1
    else if (num == "worst")
        nrow(sorted_hospitals)
    else
        num
    sorted_hospitals[, "Hospital.Name"][index]
}