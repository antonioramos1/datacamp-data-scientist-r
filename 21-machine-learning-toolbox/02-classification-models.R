# Shuffle row indices: rows
rows <- sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar <- Sonar[rows, ]

# Identify row to split on: split
split <- round(nrow(Sonar) * 0.6)

# Create train
train <- Sonar[1:split,]
# Create test
test <- Sonar[(split+1):nrow(Sonar),]


# Fit glm model: model
model <- glm(Class ~ ., train, family="binomial" )

# Predict on test: p
p <- predict(model, test, type="response")


# Calculate class probabilities: p_class
p_class <- ifelse(p > 0.5, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# Apply threshold of 0.9: p_class
p_class <- ifelse(p > 0.9, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# Apply threshold of 0.10: p_class
p_class <- ifelse(p > 0.1, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# Predict on test: p
p <- predict(model, test, type = "response")

# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)


# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)


# Train glm with custom trainControl: model
model <- train(Class ~ ., method="glm", Sonar, trControl=myControl)

# Print model to console
print(model)
