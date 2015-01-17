monitor_data <- function(directory, monitor_id) {
    monitor <- if(monitor_id < 10)
        paste("00", as.character(monitor_id), sep = "")
    else if(monitor_id < 100)
        paste("0", as.character(monitor_id), sep = "")
    else
        monitor_id
    file_dir <- paste(directory, "/", monitor, ".csv", sep = "")
    read.csv(file_dir)
}

monitors_data_c <- function(ids, monitor_reader_function) {
    if (length(ids) == 1)
        monitor_reader_function(ids[1])
    else {
        data_tail <- tail(ids, length(ids) - 1)
        c(monitor_reader_function(ids[1]),
          monitors_data_c(data_tail, monitor_reader_function))
    }
}