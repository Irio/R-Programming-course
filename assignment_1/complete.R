complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    read_monitor <- function(monitor_id) {
        data <- monitor_data(directory, monitor_id)
        appearences <- nrow(data[complete.cases(data), ])
        as.data.frame(list("id" = monitor_id, "nobs" = appearences))
    }
    
    read_monitors <- function(remaining_ids) {
        if (length(remaining_ids) == 1)
            read_monitor(remaining_ids[1])
        else {
            complete_tail <- tail(remaining_ids, length(remaining_ids) - 1)
            rbind(read_monitor(remaining_ids[1]), read_monitors(complete_tail))
        }
    }
    
    monitors_data <- read_monitors(id)
    monitors_data[complete.cases(monitors_data), ]
}