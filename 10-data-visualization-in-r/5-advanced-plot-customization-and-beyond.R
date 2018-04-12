# Create a table of Cylinders frequencies
tbl <- table(Cars93$Cylinders)

# Generate a horizontal barplot of these frequencies
mids <- barplot(tbl, horiz = TRUE,
                col = "transparent",
                names.arg = "")

# Add names labels with text()
text(20, mids, names(tbl))

# Add count labels with text()
text(35, mids, as.numeric(tbl))

# Call symbols() to create the default bubbleplot
symbols(Cars93$Horsepower, Cars93$MPG.city,
        circles = Cars93$Price^(1/2))

# Repeat, with the inches argument specified
symbols(Cars93$Horsepower, Cars93$MPG.city,
        circles = Cars93$Price^(1/2),
        inches = 0.1)

# Call png() with the name of the file we want to create
png("bubbleplot.png")

# Re-create the plot from the last exercise
symbols(Cars93$Horsepower, Cars93$MPG.city,
        circles = sqrt(Cars93$Price),
        inches = 0.1)

# Save our file and return to our interactive session
dev.off()
list.files("bubbleplot.png")

# Verify that we have created the file
list.files(pattern = "png")

# Iliinsky and Steele color name vector
IScolors <- c("red", "green", "yellow", "blue",
              "black", "white", "pink", "cyan",
              "gray", "orange", "brown", "purple")

# Create the data for the barplot
barWidths <- c(rep(2, 6), rep(1, 6))

# Recreate the horizontal barplot with colored bars
barplot(rev(barWidths), horiz = TRUE,
        col = rev(IScolors), axes = FALSE,
        names.arg = rev(IScolors), las = 1)

# Iliinsky and Steele color name vector
IScolors <- c("red", "green", "yellow", "blue",
              "black", "white", "pink", "cyan",
              "gray", "orange", "brown", "purple")

# Create the `cylinderLevel` variable
cylinderLevels <- as.numeric(Cars93$Cylinders)

# Create the colored bubbleplot
symbols(Cars93$Horsepower, Cars93$MPG.city, 
        circles = cylinderLevels, inches = 0.2, 
        bg = IScolors[cylinderLevels])

# Create a table of Cylinders by Origin
tbl <- table(Cars93$Cylinders, Cars93$Origin)

# Create the default stacked barplot
barplot(tbl)

# Enhance this plot with color
barplot(tbl,col=IScolors)

# Load the insuranceData package
library(insuranceData)

# Use the data() function to load the dataCar data frame
data(dataCar)

# Load the tabplot package
suppressPackageStartupMessages(library(tabplot))

# Generate the default tableplot() display
tableplot(dataCar)

# Load the lattice package
library(lattice)

# Use xyplot() to construct the conditional scatterplot
xyplot(calories ~ sugars | as.factor(shelf), data= UScereal)

# Load the ggplot2 package
library(ggplot2)

# Create the basic plot (not displayed): basePlot
basePlot <- ggplot(Cars93, aes(x = Horsepower, y = MPG.city))

# Display the basic scatterplot
basePlot + 
  geom_point()

# Color the points by Cylinders value
basePlot + 
  geom_point(colour = IScolors[Cars93$Cylinders])

# Make the point sizes also vary with Cylinders value
basePlot + 
  geom_point(colour = IScolors[Cars93$Cylinders], 
             size = as.numeric(Cars93$Cylinders))


