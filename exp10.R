# Sample Data: Sales over 6 months
sales <- c(100, 120, 140, 160, 180, 200)

# Create a time series object (monthly data)
time_series <- ts(sales, frequency = 12, start = c(2023, 1))

# Plot the time series
plot(time_series, main="Sales Time Series", xlab="Month", ylab="Sales", col="blue")

# Fit a simple linear regression trend
linear_model <- lm(sales ~ c(1:6))  # Simple time-based linear regression
abline(linear_model, col="red")  # Add trend line to plot




# Sample Data: Sales vs Time (quadratic relationship)
time <- c(1, 2, 3, 4, 5)
sales <- c(50, 60, 75, 95, 120)  # Quadratic growth

# Plot the data
plot(time, sales, main="Sales vs Time", xlab="Time", ylab="Sales", pch=16)

# Fit a quadratic model (Non-linear Least Squares)
nls_model <- nls(sales ~ a * time^2 + b * time + c, start = list(a = 1, b = 1, c = 50),
                 control = nls.control(maxiter = 200))

# Add the fitted curve to the plot
curve(predict(nls_model, list(time = x)), add = TRUE, col = "red")
  




# Sample Data: Sales vs Time
time <- c(1, 2, 3, 4, 5)
sales <- c(50, 60, 75, 95, 120)  # Quadratic growth

# Create a simple data frame
data <- data.frame(time, sales)

# Load the rpart library
library(rpart)

# Fit the decision tree model with adjusted control parameters
tree_model <- rpart(sales ~ time, data = data, control = rpart.control(minsplit = 2, cp = 0.01))

# Plot the decision tree
plot(tree_model)
text(tree_model, use.n = TRUE)

# Predict using the decision tree
predicted_sales <- predict(tree_model, newdata = data)

# Plot actual vs predicted sales
plot(time, sales, main="Decision Tree Regression", xlab="Time", ylab="Sales", pch=16)
lines(time, predicted_sales, col="red", lwd=2)
