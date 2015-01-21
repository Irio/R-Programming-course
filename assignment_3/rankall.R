source("best.R")

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    data <- outcome_of_care_measures(outcome)
    sorted_hospitals <- data[order(data$State, data["outcome"], data$Hospital.Name), ]
    
    index <- function(hospital_entries)
        if (num == "best")
            hospital_entries[1]
        else if (num == "worst")
            tail(hospital_entries, 1)
        else
            hospital_entries[num]
    
    hospital_in_index <- aggregate(sorted_hospitals, list(State = sorted_hospitals$State), FUN = index)
    useful_variables <- hospital_in_index[, c("Hospital.Name", "State")]
    names(useful_variables) <- c("hospital", "state")
    useful_variables
}