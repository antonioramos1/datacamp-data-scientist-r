# logs is available in your workspace

# Call max() on timestamps
max(timestamps)

# What is the date of the latest timestamp?
as.Date(max(timestamps))

# A faulty version of timestamps is available in your workspace

# Print out timestamps
timestamps

# Call max() on timestamps, no additional arguments
max(timestamps)

# Call max() on timestamps, specify na.rm
max(timestamps, na.rm=TRUE)

# logs is available in your workspace

# for loop to extract timestamp; put this inside function body below
info <- c()
for (log in logs) {
 info <- c(info, log$timestamp)
}

# Build a function extract_info(): use for loop, add return statement
extract_info <- function(x) {
  info <- c()
  for (log in x) {
  info <- c(info, log$timestamp)
  }
return(info)
}

# Call extract_info() on logs
extract_info(logs)

# logs is available in your workspace

# Adapt the extract_info() function.
extract_info <- function(x, property) {
  info <- c()
  for (log in x) {
   info <- c(info, log[[property]])
  }
  return(info)
}

# Call extract_info() on logs, set property to "timestamp"
extract_info(logs,"timestamp")

# Call extract_info() on logs, set property to "success"
extract_info(logs, "success")

# logs is available in your workspace

# Add default value for property argument
extract_info <- function(x, property = "success") {
  info <- c()
  for (log in x) {
   info <- c(info, log[[property]])
  }
  return(info)
}

# Call extract_info() on logs, don't specify property
extract_info(logs)

#< Call extract_info() on logs, set property to "timestamp"
extract_info(logs, "timestamp")

# logs is available in your workspace

# Adapt extract_info():
# - add argument with default value
# - change function body
extract_info <- function(x, property = "success", include_all = TRUE) {
  info <- c()
  for (log in x) {

   # add if construct around the line below
   if (include_all == TRUE | log$success == FALSE ){
   info <- c(info, log[[property]])
  }
  }
  return(info)
}

# Call extract_info() on logs, no additional arguments
extract_info(logs)

# Call extract_info() on logs, set include_all to FALSE
extract_info(logs, include_all = FALSE)

# logs is available in your workspace

# Defition of the extract_info() function
extract_info <- function(x, property = "success", include_all = TRUE) {
  info <- c()
  for (log in x) {
    if (include_all || !log$success) {
     info <- c(info, log[[property]])
    }
  }
  return(info)
}

# Generate vector of messages
extract_info(logs, property = c("details", "message") )

# Generate vector of locations for failed log entries
extract_info(logs, property = c("details", "location") , include_all = FALSE)

# logs is available in your workspace

# Write the function compute_fail_pct
compute_fail_pct <- function(list_of_log_entries) {
  number_of_failures <- 0
  for (log_entry in list_of_log_entries) {
    if (!log_entry$success) {
      number_of_failures <- number_of_failures + 1
    }
  }
  number_of_log_entries <- length(list_of_log_entries)
  fraction_of_failures <- number_of_failures / number_of_log_entries
  percentage_of_failures <- 100 * fraction_of_failures
  return(percentage_of_failures)
}

# Call compute_fail_pct() on logs
compute_fail_pct(logs)


