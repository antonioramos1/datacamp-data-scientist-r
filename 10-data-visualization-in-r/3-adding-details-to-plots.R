# Assign the return value from the par() function to plot_pars
plot_pars <- par()

# Display the names of the par() function's list elements
names(plot_pars)

# Display the number of par() function list elements
length(plot_pars)

# Set up a 2-by-2 plot array
par(mfrow=c(2,2))

# Plot the Animals2 brain weight data as points
plot(Animals2$brain, type="p")

# Add the title
title("points")

# Plot the brain weights with lines
plot(Animals2$brain, type="l")

# Add the title
title("lines")

# Plot the brain weights as lines overlaid with points
plot(Animals2$brain, type="o")

# Add the title
title("overlaid")

# Plot the brain weights as steps
plot(Animals2$brain, type="s")

# Add the title
title("steps")

# Compute max_hp
max_hp <- max(Cars93$Horsepower,mtcars$hp)

# Compute max_mpg
max_mpg <- max(Cars93$MPG.city, Cars93$MPG.highway,mtcars$mpg)

# Create plot with type = "n"               
plot(Cars93$Horsepower, mtcars$City,type = "n", xlim = c(0, max_hp),ylim = c(0,max_mpg), xlab = "Horsepowers",ylab = "Miles per gallon")

# Add open circles to plot
points(mtcars$hp, mtcars$mpg, pch = 1)

# Add solid squares to plot
points(Cars93$Horsepower, Cars93$MPG.city, pch = 15)

# Add open triangles to plot
points(Cars93$Horsepower, Cars93$MPG.highway, pch = 2)

# Create the numerical vector x
x <- seq(0, 10, length = 200)

# Compute the Gaussian density for x with mean 2 and standard deviation 0.2
gauss1 <- dnorm(x, mean = 2, sd = 0.2)

# Compute the Gaussian density with mean 4 and standard deviation 0.5
gauss2 <- dnorm(x, mean = 4, sd = 0.5)

# Plot the first Gaussian density
plot(x, gauss1, type = "l", ylab = "Gaussian probability density")

# Add lines for the second Gaussian density
lines(x, gauss2, lty = 2, lwd = 3)

# Create an empty plot using type = "n"
plot(mtcars$hp, mtcars$mpg, type="n", xlab="Horsepower",ylab="Gas mileage")

# Add points with shapes determined by cylinder number
points(mtcars$hp, mtcars$mpg, pch = mtcars$cyl)

# Create a second empty plot
plot(mtcars$hp, mtcars$mpg, type="n", xlab="Horsepower",ylab="Gas mileage")


# Add points with shapes as cylinder characters
points(mtcars$hp, mtcars$mpg, pch = as.character(mtcars$cyl))

# Build a linear regression model for the whiteside data
linear_model <- lm(Gas ~ Temp, whiteside)
# Create a Gas vs. Temp scatterplot from the whiteside data
plot(whiteside$Temp, whiteside$Gas)

# Use abline() to add the linear regression line
abline(linear_model, lty=2)

# Create MPG.city vs. Horsepower plot with solid squares
plot(Cars93$Horsepower, Cars93$MPG.city, pch = 15)

# Create index3, pointing to 3-cylinder cars
index3 <- which(Cars93$Cylinders == 3)

# Add text giving names of cars next to data points
text(x = Cars93$Horsepower[index3], 
     y = Cars93$MPG.city[index3],
    labels = Cars93$Make[index3], adj = 0)

# Plot MPG.city vs. Horsepower as open circles
plot(Cars93$Horsepower, Cars93$MPG.city,pch=1)

# Create index3, pointing to 3-cylinder cars
index3 <- which(Cars93$Cylinders == 3)

# Highlight 3-cylinder cars as solid circles
points(Cars93$Horsepower[index3], Cars93$MPG.city[index3], pch=16)

# Add car names, offset from points, with larger bold text
text(x=Cars93$Horsepower[index3], y=Cars93$MPG.city[index3], labels=Cars93$Make[index3], font=4, cex=1.2, adj=-0.2)

# Plot Gas vs. Temp as solid triangles
plot(whiteside$Temp, whiteside$Gas, pch = 17)

# Create indexB, pointing to "Before" data
indexB <- which(whiteside$Insul == "Before")

# Create indexA, pointing to "After" data
indexA <- which(whiteside$Insul == "After")

# Add "Before" text in blue, rotated 30 degrees, 80% size
text(x = whiteside$Temp[indexB], y = whiteside$Gas[indexB],
     labels = "Before", col = "blue", srt = 30, cex = 0.8)

# Add "After" text in red, rotated -20 degrees, 80% size
text(x = whiteside$Temp[indexA], y = whiteside$Gas[indexA],
     labels = "After", col = "red", srt = -20, cex = 0.8)

# Set up and label empty plot of Gas vs. Temp
plot(whiteside$Temp, whiteside$Gas,
     type = "n", xlab = "Outside temperature",
     ylab = "Heating gas consumption")

# Create indexB, pointing to "Before" data
indexB <- which(whiteside$Insul == "Before")

# Create indexA, pointing to "After" data
indexA <- which(whiteside$Insul == "After")

# Add "Before" data as solid triangles
points(whiteside$Temp[indexB], whiteside$Gas[indexB], pch=17)

# Add "After" data as open circles
points(whiteside$Temp[indexA], whiteside$Gas[indexA], pch=1)

# Add legend that identifies points as "Before" and "After"
legend("topright", pch = c(17,1), 
       legend = c("Before", "After"))

# Create a boxplot of sugars by shelf value, without axes
boxplot(sugars ~ shelf, data=UScereal, axes=FALSE)

# Add a default y-axis to the left of the boxplot
axis(side = 2)

# Add an x-axis below the plot, labelled 1, 2, and 3
axis(side = 1, at = c(1,2,3), labels=c(1,2,3))

# Add a second x-axis above the plot
axis(side = 3, at = c(1,2,3),
     labels = c("floor", "middle","top"))

# Create a scatterplot of MPG.city vs. Horsepower
plot(Cars93$Horsepower, Cars93$MPG.city)

# Call supsmu() to generate a smooth trend curve, with default bass
trend1 <- supsmu(Cars93$Horsepower, Cars93$MPG.city)

# Add this trend curve to the plot
lines(trend1)

# Call supsmu() for a second trend curve, with bass = 10
trend2 <- supsmu(Cars93$Horsepower, Cars93$MPG.city,bass=10)

# Add this trend curve as a heavy, dotted line
lines(trend2,lty=3,lwd=2)


