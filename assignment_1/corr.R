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
        monitor <- if(monitor_id < 10) {
            paste("00", as.character(monitor_id), sep = "")
        } else if(monitor_id < 100) {
            paste("0", as.character(monitor_id), sep = "")
        } else {
            monitor_id
        }
        file_dir <- paste(directory, "/", monitor, ".csv", sep = "")
        data <- read.csv(file_dir)[, c("sulfate", "nitrate")]
        cor(data, use = "complete.obs")[[2]]
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
    
    meets_threshold <- complete(directory)[, 2] > threshold
    if (any(meets_threshold)) {
        monitors_data <- read_monitors(which(meets_threshold))
    } else {
        numeric(0)
    }
}