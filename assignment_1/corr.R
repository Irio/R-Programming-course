corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    source("complete.R")
    
    read_monitor <- function(monitor_id) {
        data <- monitor_data(directory, monitor_id)
        filtered_dataset <- data[, c("sulfate", "nitrate")]
        cor(filtered_dataset, use = "complete.obs")[[2]]
    }
    
    meets_threshold <- complete(directory)[, 2] > threshold
    if (any(meets_threshold))
        monitors_data_c(which(meets_threshold), read_monitor)
    else
        numeric(0)
}