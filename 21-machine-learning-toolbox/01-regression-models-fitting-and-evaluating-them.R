# Fit lm model: model
model <- lm(price ~ ., diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors: error
error <- (p - diamonds[["price"]])

# Calculate RMSE
sqrt(mean(error^2))


# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
diamonds <- diamonds[rows,]


# Determine row to split on: split
split <- round(nrow(diamonds)*.80)

# Create train
train <- diamonds[1:split,]

# Create test
test <- diamonds[(split+1):nrow(diamonds),]


# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)


# Compute errors: error
error =  (p - test[["price"]])

# Calculate RMSE
sqrt(mean(error^2))



# Fit lm model using 10-fold CV: model
model <- train(
  price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
print(model)


# Fit lm model using 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

# Print model to console
print(model)


# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)

# Print model to console
print(model)


# Predict on full Boston dataset
predict(model, Boston)
