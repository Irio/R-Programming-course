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
    
    source("readers.R")
    
    read_monitor <- function(monitor_id) {
        data <- monitor_data(directory, monitor_id)
        filtered_dataset <- data[, pollutant]
        filtered_dataset[complete.cases(filtered_dataset)]
    }
    
    mean(monitors_data_c(id, read_monitor))
}