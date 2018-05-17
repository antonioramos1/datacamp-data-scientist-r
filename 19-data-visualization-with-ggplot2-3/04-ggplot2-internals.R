# Draw rectangle in null viewport
grid.rect(gp = gpar(fill = "grey90"))

# Write text in null viewport
grid.text("null viewport")

# Draw a line
grid.lines(x = c(0, 0.75), y = c(0.25, 1),
           gp = gpar(lty = 2, col = "red"))


# Populate null viewport
grid.rect(gp = gpar(fill = "grey90"))
grid.text("null viewport")
grid.lines(x = c(0,0.75), y = c(0.25, 1),
           gp = gpar(lty = 2, col = "red"))

# Create new viewport: vp
vp <- viewport(x = 0.5, y = 0.5, width = 0.5, height = 0.5, just = "center")

# Push vp
pushViewport(vp)

# Populate new viewport with rectangle
grid.rect(gp = gpar(fill = "blue"))


# 1 - Create plot viewport: pvp
mar <- c(5, 4, 2, 2)
pvp <- plotViewport(mar)

# 2 - Push pvp
pushViewport(pvp)

# 3 - Add rectangle
grid.rect(gp = gpar(fill = "grey80"))

# Create data viewport: dvp
dvp <- dataViewport(xData = mtcars$wt, yData = mtcars$mpg)

# 4 - Push dvp
pushViewport(dvp)

# Add two axes
grid.xaxis()
grid.yaxis()


# Work from before
pushViewport(plotViewport(c(5, 4, 2, 2)))
grid.rect(gp = gpar())
pushViewport(dataViewport(xData = mtcars$wt, yData = mtcars$mpg))
grid.xaxis()
grid.yaxis()

# 1 - Add text to x axis
grid.text("Weight", y = unit(-3, "lines"))

# 2 - Add text to y axis
grid.text("MPG", x = unit(-3, "lines"), rot = 90)

# 3 - Add points
grid.points(x = mtcars$wt, y = mtcars$mpg, pch=16)



# Work from before
pushViewport(plotViewport(c(5, 4, 2, 2)))
grid.rect(gp = gpar())
pushViewport(dataViewport(xData = mtcars$wt, yData = mtcars$mpg))
grid.xaxis()
grid.yaxis()

# Work from before - add names
grid.text("Weight", y = unit(-3, "lines"), name = "xaxis")
grid.text("MPG", x = unit(-3, "lines"), rot = 90, name = "yaxis")
grid.points(x = mtcars$wt, y = mtcars$mpg, pch = 16, name = "datapoints")

# Edit "xaxis"
grid.edit("xaxis",label="Weight (1000 lbs)")

# Edit "yaxis"
grid.edit("yaxis",label="Miles/(US) gallon")

# Edit "datapoints"
grid.edit("datapoints", gp = gpar(col = "#C3212766", cex = 2))


# A simple plot p
p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) + geom_point()

# Create gtab with ggplotGrob()
gtab <- ggplotGrob(p)

# Print out gtab
gtab

# Extract the grobs from gtab
g <- gtab$grob

# Draw only the legend
legend_index <- which(vapply(g, inherits, what = "gtable", logical(1)))
grid.draw(g[[legend_index]])


# Code from before
p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) + geom_point()
gtab <- ggplotGrob(p)
g <- gtab$grobs
legend_index <- which(vapply(g, inherits, what = "gtable", logical(1)))
grid.draw(g[[legend_index]])

# 1 - Show layout of legend grob
gtable_show_layout(g[[legend_index]])

# Create text grob
my_text <- textGrob(label = "Motor Trend, 1974", gp = gpar(fontsize = 7, col = "gray25"))

# 2  -Use gtable_add_grob to modify original gtab
new_legend <- gtable_add_grob(gtab$grobs[[legend_index]], my_text, 3, 2)

# 3 - Update in gtab
gtab$grobs[[legend_index]] <- new_legend

# 4 - Draw gtab
grid.draw(gtab)


# Simple plot p
p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) + geom_point()

# Examine class() and names()
class(p)
names(p)

# Print the scales sub-list
print(p$scales$scales)

# Update p
p <- p +
  scale_x_continuous("Length", limits = c(4, 8), expand = c(0, 0)) +
  scale_y_continuous("Width", limits = c(2, 4.5), expand = c(0, 0))

# Print the scales sub-list
print(p$scales$scales)


# Box plot of mtcars: p
p <- ggplot(mtcars, aes(x = factor(cyl), y = wt)) + geom_boxplot()

# Create pbuild
pbuild <- ggplot_build(p)

# a list of 3 elements
names(pbuild)

# Print out each element in pbuild
pbuild$data
pbuild$layout
pbuild$plot

# Create gtab from pbuild
gtab <- ggplot_gtable(pbuild)

# Draw gtab
grid.draw(gtab)


# Box plot of mtcars: p
p <- ggplot(mtcars, aes(x = factor(cyl), y = wt)) + geom_boxplot()

# Build pdata
pdata <- ggplot_build(p)$data

# confirm that the first element of the list is a data frame
class(pdata[[1]])

# Isolate this data frame
my_df <- pdata[[1]]

# The x labels
my_df$group <- c("4", "6", "8")

# Print out specific variables
my_df[c(1:6, 11)]


# Add a theme (legend at the bottom)
g1 <- ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(alpha = 0.5) +
  theme(legend.position = "bottom")

# Add a theme (no legend)
g2 <- ggplot(mtcars, aes(disp, fill = cyl)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 20) +
  theme(legend.position = "none")

# Load gridExtra
library(gridExtra)

# Call grid.arrange()
grid.arrange(g1, g2, ncol = 2)


# ggplot2, grid and gridExtra have been loaded for you
# Definitions of g1 and g2
g1 <- ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point() +
  theme(legend.position = "bottom")

g2 <- ggplot(mtcars, aes(disp, fill = cyl)) +
  geom_histogram(binwidth = 20) +
  theme(legend.position = "none")

# Extract the legend from g1
my_legend <- ggplotGrob(g1)$grobs[[legend_index]]  

# Create g1_noleg
g1_noleg <- g1 + 
    theme(legend.position = "none")

# Calculate the height: legend_height
legend_height <- sum(my_legend$heights)

# Arrange g1_noleg, g2 and my_legend
grid.arrange(g1_noleg, g2, my_legend,
             layout_matrix = matrix(c(1,3,2,3), ncol = 2),
             heights = unit.c(unit(1, "npc") - legend_height, legend_height))

