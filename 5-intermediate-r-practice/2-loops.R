# logs is already available in your workspace

# Print the structure of logs
str(logs)

# Use list subsetting to print the details part of 11th logs entry
logs[[11]][2]

# Print the class of the timestamp component of the first entry
class(logs[[1]]$timestamp)

# logs is available in your workspace

# Initialize the iterator i to be 1
i <- 1

# Code the while loop
while (logs[[i]][1]){
  print(i)
  i <- i + 1
}

# logs is available in your workspace

# Adapt the while loop
i <- 1
while (logs[[i]]$success) {
  print(logs[[i]]$details$message)
  i <- i + 1
}

# logs is available in your workspace

# Initialize i and found
i <- 1
found <- FALSE

# Code the while loop
while (found == FALSE) {
  if (logs[[i]]$success == FALSE && logs[[i]]$details$location == "waste") {
    print("found")
    found <- TRUE
  } else {
    print("still looking")
    i <- i+1
  }
}

# logs is available in your workspace

# Code a for loop that prints the timestamp of each log
for (log in logs) {
  print(log[[3]])
}



# logs is available in your workspace

# Make the printout conditional: only if success
for (log in logs) {
  if (log$success == TRUE){
  print(log$timestamp)  
  }
}


# logs is available in your workspace

# Finish the for loop: add date element for each entry
for (i in 1:length(logs)) {
  logs[[i]]$date <- as.Date(logs[[i]]$timestamp)
}

# Print first 6 elements in logs
head(logs)

# logs is available in your workspace

# Intialize empty list: failures
failures <- list()

# Finish the for loop: add each failure to failures
for (log in logs) {
  if (log$success == FALSE) {
    failures <- c(failures, list(log))
  }
}

# Display the structure of failures
str(failures)
