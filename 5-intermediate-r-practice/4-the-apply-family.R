# logs is available in your workspace

# Call length() on each element of logs
lapply(logs, length)

# Call class() on each element of logs
lapply(logs, class)

# logs is available in your workspace

# Define get_timestamp()
get_timestamp <- function(x) {
 return(x[["timestamp"]])
}

# Apply get_timestamp() over all elements in logs
lapply(logs, get_timestamp)

# logs is available in your workspace

# Have lapply() use an anonymous function
lapply(logs, function(x){x$timestamp})

# logs is available in your workspace

# Call length() on each element of logs using sapply()
sapply(logs, length)

# Definition of get_timestamp
get_timestamp <- function(x) {
  x$timestamp
}

# Get vector of log entries' timestamps
sapply(logs, get_timestamp)

# logs is available in your workspace

# Use sapply() to select the success element from each log: results
results <- sapply(logs, function(x){x$success})

# Call mean() on results
mean(results)

# Use sapply() to select the details element from each log
sapply(logs, function(x){x$details})

# logs is available in your workspace

# Implement function get_failure_loc
get_failure_loc <- function(x) {
  if (x$success) {
    return(NULL)
  } else {
    return(x$details$location)
  }
}

# Use sapply() to call get_failure_loc on logs
sapply(logs, get_failure_loc)

# logs is available in your workspace

# Convert the sapply call to vapply
vapply(logs, length, numeric(1))

# Convert the sapply call to vapply
vapply(logs, `[[`, "success", FUN.VALUE = logical(1))

# logs is available in your workspace

# Convert the sapply() call to a vapply() or lapply() call
vapply(logs, `[[`, c("details", "message"), FUN.VALUE = character(1))

# Convert the sapply() call to a vapply() or lapply() call
lapply(logs, function(x) { x$details })

# logs is available in your workspace

# Return vector with uppercase version of message elements in log entries
vapply(logs, function(x){toupper(x$details$message)}, character(1))


