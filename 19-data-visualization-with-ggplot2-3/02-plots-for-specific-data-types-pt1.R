# pairs
pairs(iris[1:4])

# chart.Correlation
library(PerformanceAnalytics)
chart.Correlation(iris[1:4])

# ggpairs
library(GGally)
ggpairs(mtcars_fact[1:3])


library(ggplot2)
library(reshape2)

cor_list <- function(x) {
  L <- M <- cor(x)
  
  M[lower.tri(M, diag = TRUE)] <- NA
  M <- melt(M)
  names(M)[3] <- "points"
  
  L[upper.tri(L, diag = TRUE)] <- NA
  L <- melt(L)
  names(L)[3] <- "labels"
  
  merge(M, L)
}

# Calculate xx with cor_list
library(dplyr)
xx <- iris %>%
  group_by(Species) %>%
  do(cor_list(.[1:4])) 

# Finish the plot
ggplot(xx, aes(x = Var1, y = Var2)) +
  geom_point(aes(col = points, size = abs(points)), shape = 16) +
  geom_text(aes(col = labels,  size = abs(labels), label = round(labels, 2))) +
  scale_size(range = c(0, 6)) +
  scale_color_gradient2("r", limits = c(-1, 1)) +
  scale_y_discrete("", limits = rev(levels(xx$Var1))) +
  scale_x_discrete("") +
  guides(size = FALSE) +
  geom_abline(slope = -1, intercept = nlevels(xx$Var1) + 1) +
  coord_fixed() +
  facet_grid(. ~ Species) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank())


# Explore africa
str(africa)
str(africa_sample)

# Add an ID column from the row.names
africa_sample$ID <- row.names(africa_sample)

# Gather africa_sample
library(tidyr)
africa_sample_tidy <- gather(africa_sample, key, value, -ID)
head(africa_sample_tidy)

# Finish the ggplot command
ggplot(africa_sample_tidy, aes(x = factor(ID), y = value, fill = key)) +
  geom_col() +
  coord_flip()


# Load ggtern
library(ggtern)

# Build ternary plot
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(shape=16, alpha=0.2)


# ggtern and ggplot2 are loaded
# Original plot:
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(shape = 16, alpha = 0.2)

# Plot 1
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  geom_density_tern()

# Plot 2
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  stat_density_tern(geom = 'polygon', aes(fill = ..level.., alpha = ..level..)) +
  guides(fill = FALSE)


# Load geomnet & examine structure of madmen
library(geomnet)
str(madmen)

# Merge edges and vertices
mmnet <- merge(madmen$edges, madmen$vertices,
               by.x = "Name1", by.y = "label",
               all = TRUE)

# Examine structure of mmnet
str(mmnet)


# geomnet is pre-loaded

# Merge edges and vertices
mmnet <- merge(madmen$edges, madmen$vertices,
               by.x = "Name1", by.y = "label",
               all = TRUE)

# Finish the ggplot command
ggplot(data = mmnet, aes(from_id = Name1, to_id = Name2)) +
  geom_net(aes(col=Gender), size=6, linewidth=1, labelon=TRUE, fontsize=3, labelcolour="black")


# geomnet is pre-loaded and mmnet is defined
head(mmnet)

# Node colors
pink_and_blue <- c(female = "#FF69B4", male = "#0099ff")

# Tweak the network plot
ggplot(data = mmnet, aes(from_id = Name1, to_id = Name2)) +
  geom_net(aes(col = Gender),
           size = 6,
           linewidth = 1,
           labelon = TRUE,
           fontsize = 3,
           labelcolour = "black",
           # Make the graph directed
           directed = TRUE) +
  # Add manual color scale
  scale_color_manual(values = pink_and_blue) + 
  # Set x-axis limits
  xlim(c(-0.05, 1.05)) +
  # Set void theme
  theme_void()



# Create linear model: res
res <- lm(Volume ~Girth, data = trees)

# Plot res
plot(res)

# Import ggfortify and use autoplot()
library(ggfortify)
autoplot(res, ncol=2)


# ggfortify and Canada are available

# Inspect structure of Canada
str(Canada)

# Call plot() on Canada
plot(Canada)

# Call autoplot() on Canada
autoplot(Canada)


# ggfortify and eurodist are available
# Autoplot + ggplot2 tweaking
autoplot(eurodist) +
  coord_fixed()
  
# Autoplot of MDS
autoplot(cmdscale(eurodist, eig = TRUE),
         label = TRUE, 
         label.size = 3, 
         size = 0)


# Perform clustering
iris_k <- kmeans(iris[-5], 3)

# Autplot: color according to cluster
autoplot(iris_k, data = iris, frame = TRUE)

# Autoplot: above, plus shape according to species
autoplot(iris_k, data = iris, frame = TRUE, shape = 'Species')


