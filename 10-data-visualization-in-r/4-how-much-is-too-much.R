# Compute the number of plots to be displayed
ncol(Cars93)^2

# Plot the array of scatterplots
plot(Cars93)

# Construct the vector keep_vars
keep_vars <- c("calories", "protein", "fat",
               "fibre", "carbo", "sugars")

# Use keep_vars to extract the desired subset of UScereal
df <- UScereal[, keep_vars]

# Set up a two-by-two plot array
par(mfrow=c(2,2))

# Use matplot() to generate an array of two scatterplots
matplot(UScereal$calories, UScereal[c("protein", "fat")],xlab="calories",ylab="")
title("Two scatterplots")

# Use matplot() to generate an array of three scatterplots
matplot(UScereal$calories, UScereal[c("protein", "fat", "fibre")],xlab="calories",ylab="")
title("Three scatterplots")

# Use matplot() to generate an array of four scatterplots
matplot(UScereal$calories, UScereal[c("protein", "fat","fibre","carbo")],xlab="calories",ylab="")
title("Four scatterplots")

# Use matplot() to generate an array of five scatterplots
matplot(UScereal$calories, UScereal[c("protein", "fat","fibre","carbo","sugars")],xlab="calories",ylab="")
title("Five scatterplots")

# Create mfr_table of manufacturer frequencies
mfr_table <- table(Cars93$Manufacturer)

# Create the default wordcloud from this table
wordcloud(words = names(mfr_table), 
          freq = as.numeric(mfr_table), 
          scale = c(2,0.25))

# Change the minimum word frequency
wordcloud(words = names(mfr_table),
          freq = as.numeric(mfr_table), 
          scale = c(2,0.25), 
          min.freq = 1)

# Create model_table of model frequencies
model_table <- table(Cars93$Model)

# Create the wordcloud of all model names with smaller scaling
wordcloud(words = names(model_table), 
          freq = as.numeric(model_table),
          scale = c(0.75,0.25), 
          min.freq = 1)

# Set up a two-by-two plot array
par(mfrow=c(2,2))

# Plot y1 vs. x1 
plot(anscombe$x1, anscombe$y1)

# Plot y2 vs. x2
plot(anscombe$x2, anscombe$y2)

# Plot y3 vs. x3
plot(anscombe$x3, anscombe$y3)

# Plot y4 vs. x4
plot(anscombe$x4, anscombe$y4)

# Define common x and y limits for the four plots
xmin <- 4
xmax <- 19
ymin <- 3
ymax <- 13

# Set up a two-by-two plot array
par(mfrow=c(2,2))

# Plot y1 vs. x1 with common x and y limits, labels & title
plot(anscombe$x1, anscombe$y1,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x value", ylab = "y value",
     main = "First dataset")

# Do the same for the y2 vs. x2 plot
plot(anscombe$x2, anscombe$y2,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x value", ylab = "y value",
     main = "Second dataset")

# Do the same for the y3 vs. x3 plot
plot(anscombe$x3, anscombe$y3,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x value", ylab = "y value",
     main = "Third dataset")

# Do the same for the y4 vs. x4 plot
plot(anscombe$x4, anscombe$y4,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x value", ylab = "y value",
     main = "Fourth dataset")

# Set up a two-by-two plot array
par(mfrow=c(2,2))

# Plot the raw duration data
plot(geyser$duration, main="Raw data")

# Plot the normalized histogram of the duration data
truehist(geyser$duration, main="Histogram")

# Plot the density of the duration data
plot(density(geyser$duration), main="Density")

# Construct the normal QQ-plot of the duration data
qqPlot(geyser$duration, main="QQ-plot")

# Use the matrix function to create a matrix with three rows and two columns
layoutMatrix <- matrix(
  c(
    0, 1,
    2, 0,
    0, 3
  ), 
  byrow = TRUE, 
  nrow = 3
)
# Call the layout() function to set up the plot array
layout(layoutMatrix)

# Show where the three plots will go 
layout.show(3)

# Set up the plot array
layout(layoutMatrix)

# Construct the vectors indexB and indexA
indexB <- which(whiteside$Insul == "Before")
indexA <- which(whiteside$Insul == "After")

# Create plot 1 and add title
plot(whiteside$Temp[indexB], whiteside$Gas[indexB],
     ylim = c(0, 8))
title("Before data only")

# Create plot 2 and add title
plot(whiteside$Temp, whiteside$Gas,
     ylim = c(0, 8))
title("Complete dataset")

# Create plot 3 and add title
plot(whiteside$Temp[indexA], whiteside$Gas[indexA],
     ylim = c(0, 8))
title("After data only")

# Create row1, row2, and layoutVector
row1 <- c(1, 0, 0)
row2 <- c(0, 2, 2)
layoutVector <- c(row1, rep(row2, 2))

# Convert layoutVector into layoutMatrix
layoutMatrix <- matrix(layoutVector, byrow = TRUE, nrow = 3)

# Set up the plot array
layout(layoutMatrix)

# Plot scatterplot
plot(Boston$rad, Boston$zn)

# Plot sunflower plot
sunflowerplot(Boston$rad, Boston$zn)


