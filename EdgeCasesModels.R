#running logistic regression on the edge and non edge cases for market return

# Filter the dataset to keep only observations with excess_return between -0.1 and 0.1
train_output_dt_edge_cases <-  train_output_dt %>%
  filter(excess_return >= -0.01 & excess_return <= 0.01)
test_output_dt_edge_cases <- test_output_dt %>%
  filter(excess_return >= -0.01 & excess_return <= 0.01)

#the other cases
train_output_dt_non_edge_cases <- train_output_dt %>%
  filter(excess_return < -0.01 | excess_return > 0.01)
test_output_dt_non_edge_cases <- test_output_dt %>%
  filter(excess_return < -0.01 | excess_return > 0.01)

rm(test_output_dt_edge_Cases)



train_output_dt_edge_cases <- train_output_dt_edge_cases %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))

train_output_dt_non_edge_cases <- train_output_dt_non_edge_cases %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))

test_output_dt_edge_Cases <- test_output_dt_edge_cases %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))

test_output_dt_non_edge_cases <- test_output_dt_non_edge_cases %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))
#1 is above, 0 below


above_count <- sum(train_output_dt_edge_cases$excessreturn_binary == "Above")
below_count <- sum(train_output_dt_edge_cases$excessreturn_binary == "Below")


# Print the result
print(paste("Number of times 'Above' appears:", above_count))
print(paste("Number of times 'Above' appears:", below_count))

# Create a data frame for plotting
pie_data <- data.frame(
  Category = c("Above", "Below"),
  Count = c(above_count, below_count)
)

# Create the pie chart
ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Converts bar chart to pie chart
  labs(title = "Distribution of Excess Return Categories") +
  theme_void() +  # Removes background grid
  scale_fill_manual(values = c("Above" = "blue", "Below" = "skyblue"))

# histogram 
hist(train_output_dt_edge_cases$excess_return, 
     main = "Histogram of Excess Return", 
     xlab = "Excess Return", 
     col = "lightblue", 
     border = "black")

#no class disparity

#doing logistic reg on the big data set

##### MODEL ###

train_output_dt_edge_cases$excessreturn_binary <- factor(train_output_dt_edge_cases$excessreturn_binary, levels = c("Below", "Above"))
test_output_dt_edge_Cases$excessreturn_binary <- factor(test_output_dt_edge_Cases$excessreturn_binary, levels = c("Below", "Above"))
#this line is to run a Logistic Regression model
train_output_dt_edge_cases <- train_output_dt_edge_cases %>%
  select(excessreturn_binary, all_of(predictor_vars)) %>%
  na.omit()
test_output_dt_edge_Cases <- test_output_dt_edge_Cases %>%
  select(excessreturn_binary, all_of(predictor_vars)) %>%
  na.omit()
LRmodel <- glm(excessreturn_binary~ ., family='binomial', train_output_dt_edge_cases)
#this line is to eliminate irrelevant variables using a stepwise approach
LRmodelR <- step(LRmodel)
probabilitiesLR <- predict(LRmodelR, test_output_dt_edge_Cases [,-1],type= "response")
predictionLR <- ifelse(probabilitiesLR > 0.5, " Above", " Below")
#if a predicted prob is higher than 0.6 then its above, if not below
#classificationtable <- table(pred= predictionLR, test_output_dt_edge_Cases[,9])
classificationtable <- table(Predicted = predictionLR, Actual = test_output_dt_edge_Cases$excessreturn_binary)

acctestLR <- sum(diag(classificationtable))/sum(classificationtable)
acctestLR
print(classificationtable) #works super well :) 

#find optimal threshold

# Compute probabilities from the logistic regression model
probabilitiesLR <- predict(LRmodelR, test_output_dt_edge_Cases[, -1], type = "response")

# Create the ROC curve object
roc_curve <- roc(test_output_dt_edge_Cases$excessreturn_binary, probabilitiesLR, levels = c("Below", "Above"))

# Plot the ROC Curve
ggplot(data = data.frame(FPR = 1 - roc_curve$specificities, TPR = roc_curve$sensitivities),
       aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "grey") +
  labs(title = "ROC Curve for Logistic Regression",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

# Find the optimal threshold using Youden's J statistic
optimal_index <- which.max(roc_curve$sensitivities + roc_curve$specificities - 1)
optimal_threshold <- roc_curve$thresholds[optimal_index]

# Print AUC and optimal threshold
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))
print(paste("Optimal Classification Threshold:", round(optimal_threshold, 4)))
print(optimal_threshold)

# Apply the optimal threshold to make predictions
predictionLR_optimal <- ifelse(probabilitiesLR > optimal_threshold, "Above", "Below")

# Create confusion matrix for new predictions
classificationtable_optimal <- table(Predicted = predictionLR_optimal, Actual = test_output_dt_edge_Cases$excessreturn_binary)
print(classificationtable_optimal)


conf_matrix <- as.data.frame(as.table(classificationtable_optimal))

# Create heatmap
ggplot(conf_matrix, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual Class", y = "Predicted Class") +
  theme_minimal()

conf_matrix <- as.data.frame(as.table(classificationtable_optimal))

# Compute total number of observations
total_obs <- sum(conf_matrix$Freq)

# Add a column for percentage
conf_matrix$Percentage <- round((conf_matrix$Freq / total_obs) * 100, 1)  # one decimal place

# Create heatmap with percentages
ggplot(conf_matrix, aes(x = Actual, y = Predicted, fill = Percentage)) +
  geom_tile() +
  geom_text(aes(label = paste0(Percentage, "%")), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap (Percentages)", 
       x = "Actual Class", 
       y = "Predicted Class") +
  theme_minimal()

summary(LRmodelR)

# Extract coefficients from the reduced logistic regression model
coef_df <- tidy(LRmodelR)

# Remove intercept
coef_df <- coef_df[coef_df$term != "(Intercept)", ]

# Add absolute value column for sorting
coef_df$abs_estimate <- abs(coef_df$estimate)

# Order by importance (magnitude)
coef_df <- coef_df[order(coef_df$abs_estimate, decreasing = TRUE), ]

# Plot the coefficients
ggplot(coef_df, aes(x = reorder(term, abs_estimate), y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Important Features from Logistic Regression",
       x = "Features",
       y = "Coefficient Estimate") +
  scale_fill_manual(values = c("purple", "blue")) +
  theme_minimal()

# Save probabilities
predictions_LR <- data.frame(
  actual = test_output_dt$excessreturn_binary,
  predicted_prob = probabilitiesLR,
  predicted_class = predictionLR_optimal  # based on optimal threshold
)

#model completely predicts above when below
