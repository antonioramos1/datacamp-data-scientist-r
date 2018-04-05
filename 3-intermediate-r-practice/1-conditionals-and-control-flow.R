# me, other_199, and previous_4 are available in your workspace

# Merge me and other_199: my_class
my_class <- c(me, other_199)

# cbind() my_class and previous_4: last_5
last_5 <- cbind(my_class, previous_4)

# Name last_5 appropriately
nms <- paste0("year_", 1:5)
colnames(last_5) <- nms
last_5

# me, my_class and last_5 are available in your workspace

# Build histogram of my_class
hist(my_class)

# Generate summary of last_5
summary(last_5)

# Build boxplot of last_5

boxplot(last_5)

# me, my_class and last_5 are preloaded

# Is your grade equal to 72?
me == 72

# Which grades in your class are higher than 75?
my_class > 75

# Which grades in the last 5 years are below or equal to 64?
last_5 <= 64

# me, my_class and last_5 are preloaded

# How many grades in your class are higher than 75?
sum(my_class > 75)

# How many students in your class scored strictly higher than you?
sum(my_class > me)

# What's the proportion of grades below or equal to 64 in the last 5 years?
sum(last_5 <= 64)/length(last_5)

# me, my_class and last_5 are preloaded

# Is your grade greater than 87 and smaller than or equal to 89?
(me > 87 & me <= 89)

# Which grades in your class are below 60 or above 90?
(my_class < 60 | my_class > 90)

# me, my_class and last_5 are preloaded

# What's the proportion of grades in your class that is average?


# How many students in the last 5 years had a grade of 80 or 90?

# me, my_class and last_5 are preloaded

# Define n_smart
n_smart <- sum(my_class >= 80)

# Code the if-else construct
if (n_smart > 50){
  print("smart class")
} else {
  print("rather average")
}

# me, my_class and last_5 are preloaded

# Define prop_less
prop_less <- sum(my_class < me)/length(my_class)

# Code the control construct

if (prop_less > 0.9){
  print("you're among the best 10 percent")
} else if (prop_less > 0.8){
  print("you're among the best 20 percent")
} else {
  print("need more analysis")
}

# me, my_class and last_5 are preloaded

# Embedded control structure: fix the error
if (mean(my_class) < 75) {
  if (mean(my_class) > me) {
    print("average year, but still smarter than me")
  } else {
    print("average year, but I'm not that bad")
  }
} else {
  if (mean(my_class) > me) {
    print("smart year, even smarter than me")
  } else {
    print("smart year, but I am smarter")
  }
}

# me, my_class and last_5 are preloaded

# Create top_grades
top_grades <- my_class[my_class >= 85]
# Create worst_grades
worst_grades <- my_class[my_class < 65]

# Write conditional statement

if (length(top_grades) > length(worst_grades)){
  print("top grades prevail")
}


