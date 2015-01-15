pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    read_monitor <- function(monitor_id) {
        monitor <- if(monitor_id < 10) {
            paste("00", as.character(monitor_id), sep = "")
        } else if(monitor_id < 100) {
            paste("0", as.character(monitor_id), sep = "")
        }
        file_dir <- paste("~/Code/r_programming/assignment_1/", directory, "/", monitor, ".csv", sep = "")
        data <- read.csv(file_dir)
        values_for_pollutant <- data[, pollutant]
        values_for_pollutant[!is.na(values_for_pollutant)]
    }
    
    read_monitors <- function(remaining_ids) {
        if (length(remaining_ids) == 1) {
            read_monitor(remaining_ids[1])
        } else {
            complete_tail <- tail(remaining_ids, length(remaining_ids) - 1)
            list <- c(read_monitor(remaining_ids[1]), read_monitors(complete_tail))
            list
        }
    }
    
    mean(read_monitors(id))
}