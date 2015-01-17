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
        monitor <- if(monitor_id < 10) {
            paste("00", as.character(monitor_id), sep = "")
        } else if(monitor_id < 100) {
            paste("0", as.character(monitor_id), sep = "")
        } else {
            monitor_id
        }
        file_dir <- paste(directory, "/", monitor, ".csv", sep = "")
        data <- read.csv(file_dir)
        
        appearences <- nrow(data[complete.cases(data), ])
        as.data.frame(list("id" = monitor_id, "nobs" = appearences))
    }
    
    read_monitors <- function(remaining_ids) {
        if (length(remaining_ids) == 1) {
            read_monitor(remaining_ids[1])
        } else {
            complete_tail <- tail(remaining_ids, length(remaining_ids) - 1)
            list <- rbind(read_monitor(remaining_ids[1]), read_monitors(complete_tail))
            list
        }
    }
    
    monitors_data <- read_monitors(id)
    monitors_data[complete.cases(monitors_data), ]
}