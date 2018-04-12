# Set up a side-by-side plot array
par(mfrow=c(1,2))

# Create a histogram of counts with hist()
hist(Cars93$Horsepower, main="hist() plot")

# Create a normalized histogram with truehist()
truehist(Cars93$Horsepower, main= "truehist() plot")

# Create index16, pointing to 16-week chicks
index16 <- which(ChickWeight$Time == 16)

# Get the 16-week chick weights
weights <- ChickWeight$weight[index16]

# Plot the normalized histogram
truehist(weights)

# Add the density curve to the histogram
lines(density(weights))

# Load the car package to make qqPlot() available
library(car)

# Create index16, pointing to 16-week chicks
index16 <- which(ChickWeight$Time == 16)

# Get the 16-week chick weights
weights <- ChickWeight$weight[index16]

# Show the normal QQ-plot of the chick weights
qqPlot(weights)

# Show the normal QQ-plot of the Boston$tax data
qqPlot(Boston$tax)

# Set up a side-by-side plot array
par(mfrow=c(1,2))

# Create the standard scatterplot
plot(Boston$zn, Boston$rad)

# Add the title
title("Standard scatterplot")

# Create the sunflowerplot
sunflowerplot(Boston$zn, Boston$rad)


# Add the title
title("Sunflower plot")

# Create a variable-width boxplot with log y-axis & horizontal labels
boxplot(crim ~ rad, data=Boston, log="y", varwidth=TRUE,las=1 )

# Add a title
title("Crime rate vs. radial highway index")

# Create a mosaic plot using the formula interface
mosaicplot(carb ~ cyl, data=mtcars)

# Create a side-by-side boxplot summary
boxplot(Cars93$Min.Price, Cars93$Max.Price)

# Load aplpack to make the bagplot() function available
library(aplpack)

# Create a bagplot for the same two variables
bagplot(Cars93$Min.Price, Cars93$Max.Price, cex=1.20)

# Add an equality reference line
abline(0,1, lty=2)

# Load the corrplot library for the corrplot() function
library(corrplot)

# Extract the numerical variables from UScereal
numericalVars <- UScereal[, 2:10]

# Compute the correlation matrix for these variables
corrMat <- cor(numericalVars)

# Generate the correlation ellipse plot
corrplot(corrMat, method = "ellipse")

# Load the rpart library
library(rpart)

# Fit an rpart model to predict medv from all other Boston variables
tree_model <- rpart(medv ~., data=Boston)

# Plot the structure of this decision tree model
plot(tree_model)

# Add labels to this plot
text(tree_model, cex=0.7)


