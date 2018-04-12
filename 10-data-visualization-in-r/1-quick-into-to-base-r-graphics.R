# Load MASS package
library(MASS)

# Plot whiteside data
plot(whiteside)

# Plot Gas vs. Temp

plot(whiteside$Temp, whiteside$Gas, xlab="Outside temperature", ylab="Heating gas consumption")

# Apply the plot() function to Insul

plot(whiteside$Insul)

# Load the MASS package
library(MASS)

# Plot Max.Price vs. Price as red triangles
plot(Cars93$Price, Cars93$Max.Price, col="red", pch=17)

# Add Min.Price vs. Price as blue circles
points(Cars93$Price, Cars93$Min.Price, col="blue", pch=16)

# Add an equality reference line with abline()
abline(a = 0, b = 1, lty = 2)

# Load the robustbase package
library(robustbase)

# Set up the side-by-side plot array
par(mfrow = c(1, 2))

# First plot: brain vs. body in its original form
plot(Animals2$body, Animals2$brain)

# Add the first title
title("Original representation")

# Second plot: log-log plot of brain vs. body
plot(Animals2$body, Animals2$brain,
     log = "xy")

# Add the second title
title("Log-log plot")

# Load the insuranceData package
library(insuranceData)

# Use the data() function to get the dataCar data frame
data(dataCar)

# Set up a side-by-side plot array
par(mfrow = c(1, 2))

# Create a table of veh_body record counts and sort
tbl <- sort(table(dataCar$veh_body),
            decreasing = TRUE)

# Create the pie chart and give it a title
pie(tbl)
title("Pie chart")

# Create the barplot with perpendicular, half-sized labels
barplot(tbl, las = 2, cex.names = 0.5)

# Add a title
title("Bar chart")


