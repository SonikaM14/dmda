height <- c(150, 160, 170, 180, 190)
weight <- c(50, 60, 65, 75, 85)
relationship_model <- lm(weight ~ height)
print(relationship_model)
print(summary(relationship_model))
plot(height, weight, col = "green",
     main = "Height and Weight Regression",
     abline(lm(weight ~ height)),
     cex = 1.3, pch = 16,
     xlab = "Height in cm", ylab = "Weight in kg")


height_logistic <- c(150, 160, 170, 180, 190)
overweight <- ifelse(height_logistic > 165, 1, 0)
logistic_model <- glm(overweight ~ height_logistic, family = binomial)
print(summary(logistic_model))
plot(height_logistic, overweight, col = "red", pch = 16,
     main = "Logistic Regression: Overweight vs Height",
     xlab = "Height (cm)", ylab = "Overweight (0 = No, 1 = Yes)")
curve(predict(logistic_model, newdata = data.frame(height_logistic = x),
              type = "response"),
      add = TRUE, col = "blue", lwd = 2)

age <- c(25, 30, 35, 40, 45)
height_multi <- c(150, 160, 170, 180, 190)
weight_multi <- c(50, 60, 65, 75, 85)
multiple_model <- lm(weight_multi ~ height_multi + age)
print(summary(multiple_model))
plot(fitted(multiple_model), residuals(multiple_model), col = "blue", 
     pch = 16,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)


height <- c(150, 160, 170, 180, 190)
exercises <- rpois(5, lambda = exp(0.05 * height))
poisson_model <- glm(exercises ~ height, family = poisson())
summary(poisson_model)
plot(height, exercises, col = "blue", pch = 16,
     main = "Poisson Regression: Exercises per Week vs Height",
     xlab = "Height", ylab = "Exercises per Week")
lines(height, predict(poisson_model, type = "response"), col = "red", lwd = 2)