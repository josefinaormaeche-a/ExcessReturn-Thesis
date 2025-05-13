#getting necessary packages
install.packages("readr")
install.packages("psych")
install.packages("skimr")
install.packages("Hmisc")
install.packages("lubridate")
install.packages("mice")
install.packages("dplyr")
install.packages("furrr")
install.packages("ROSE")
install.packages("smotefamily")
install.packages("patchwork")
install.packages('pROC')
install.packages("xgboost")
install.packages("caret")

#loading libraries
library(psych)
library(readr)
library(dplyr)
library(skimr)
library(Hmisc)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)      
library(caret)      
library(mice)       
library(data.table) 
library(furrr)
library(plotly)
library("readxl")
library(data.table)
library(openxlsx)
library(ROSE)
library(smotefamily)
library(pROC)
library(patchwork)
library(zoo)
library(leaps)
library(glmnet)
library("tree")
library("randomForest")
library("gbm")
library(e1071)
library('FNN')
library(class)
library(reshape2)
library(pROC)
library(xgboost)
library(caret)

file_path <- "C:/Users/joseo/OneDrive/Desktop/cbs/Fourth Semester/output22.csv"
#file_path2 <- "C:/Users/joseo/OneDrive/Desktop/cbs/Fourth Semester/datashare.csv"
file_path3 <- "C:/Users/joseo/OneDrive/Desktop/cbs/Fourth Semester/DTB3.csv"

# datashare <- read.csv(file_path2) 

output <- read.csv2(file_path)

treasurybill <- read.csv(file_path3)


treasurybill <- treasurybill %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  filter(year(observation_date) >= 1999 & year(observation_date) <= 2022) %>%
  filter(!is.na(DTB3)) %>%  # remove NA observations first
  group_by(year = year(observation_date), month = month(observation_date)) %>%
  arrange(observation_date) 


#creating the filtered data set

#it is monthly data for each permno from 2001 to 2022 (not included)

#delete all observations with the same permno that have missing RET
f_output <- output %>%
  group_by(permno) %>%
  mutate(has_missing_ret_permno = any(is.na(RET))) %>%
  ungroup() %>%
  group_by(gvkey) %>%
  mutate(has_missing_ret_gvkey = any(is.na(RET))) %>%
  ungroup() %>%
  filter(!has_missing_ret_permno & !has_missing_ret_gvkey) %>%
  select(-has_missing_ret_permno, -has_missing_ret_gvkey)

# Filter dataset based on occurrence count
f_output <- f_output %>%
  group_by(permno) %>%
  filter(n() > 50) %>%  
  ungroup()

f_output <- output %>%
  group_by(permno, fyear) %>%  
  mutate(month_id = row_number()) %>%  
  ungroup() %>%
  relocate(month_id, .before = 4) %>%  
  group_by(permno)  

#descriptive statistics
#summary(output)

#Merging RF to general dataset

treasurybill <- treasurybill %>%
  mutate(year = year(observation_date)) %>%
  select(year, month, DTB3) %>%  # Ensure month_id is included
  distinct(year, month, .keep_all = TRUE) %>%  # Ensure uniqueness
  arrange(year, month)

# Convert to data.table
output_dt <- as.data.table(f_output)
treasurybill_dt <- as.data.table(treasurybill)

# keys for joining
setkey(treasurybill_dt, year, month)
setkey(output_dt, fyear, month_id)

# Rolling join based on year and month_id
output_dt <- treasurybill_dt[output_dt, roll = TRUE]

#this just generated a new data set that has the risk free rate joint 

output_dt <- output_dt %>%
  mutate(rf_monthly = DTB3 / 12) %>%
  relocate(rf_monthly, .before = 4) 
  
output_dt <- output_dt %>%
  mutate(RET_adj = ifelse(!is.na(DLRET), (1 + RET) * (1 + DLRET) - 1, RET))

# Calculating excess returns 
output_dt <- output_dt %>%
  mutate(
    rf_monthly = (DTB3), #because DTB3 is in percent
    excess_return = RET_adj - rf_monthly
  ) %>%
relocate(excess_return, .before = 5)  

# Identifying permnos with any NA or infinite values in excess_return
permnos_to_remove <- output_dt %>%
  group_by(permno) %>%
  filter(any(is.na(excess_return) | !is.finite(excess_return))) %>%
  pull(permno) %>% 
  unique()

# Remove all rows of affected permnos
output_dt_clean <- output_dt %>%
  filter(!permno %in% permnos_to_remove)

output_dt_clean <-output_dt_clean %>% arrange(permno, year,month)

# 12-month rolling standard deviation (volatility)
output_dt_clean <- output_dt_clean %>%
  arrange(permno, year, month) %>%
  group_by(permno) %>%
  mutate(volatility = if (n() >= 12) runSD(RET, n = 12) else NA_real_) %>%
  ungroup()

 #filtering one last time
### done with merging

num_unique_permno_clean <- output_dt_clean %>% distinct(permno) %>% nrow()
print(num_unique_permno_clean)
#6321 stocks total

# Calculate descriptive statistics

names(output_dt_clean)

# bubble chart for data visualization
# Filter the dataset to include only records where the fiscal year is 2020,
#could also be done on a permno base
filtered_year <- output_dt %>%
  filter(year == 2016)

#i basically want to create a monthly column here so that for any year if gives 1 to the first 
#occurance of that year and 12 to the last, fitting all the ones in between in order as well


# Create the bubble chart with ggplot2
p <- ggplot(filtered_year, aes(x = permno, y = excess_return, size = abs(excess_return), color = excess_return)) +  # Color by RET
  geom_point(alpha = 0.6) + 
  scale_size_continuous(name = "Absolute excess return", range = c(2, 10)) +  
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "Bubble Chart of Excess Return for Fiscal Year 2016",
       x = "permno",
       y = "Excess Return") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

# Convert ggplot object to a plotly interactive plot
interactive_plot <- ggplotly(p, tooltip = "permno")
interactive_plot

## PREPARING THE DATA SET FOR MODELS

# Filtering stocks where DLRET ≠ 0 and RET ≠ DLRET
different_delist_stocks <- output_dt %>%
  filter(!is.na(DLRET) & DLRET != 0 & RET != DLRET)
print(different_delist_stocks)

delisted_Stocks <- f_output %>% filter(permno == 26374)
#this was just to check it made sense to modify how ret is calculated

#to account for stocks delisted during the month, DLRET is used and taken into account as that
#is where those are stored

# Step 1 - Winsorize relevant variables 
#Winsorizing the dataset ensures that extreme values (outliers) are capped 
#at the 1st percentile (lower bound) and 99th percentile (upper bound) to make
#the data more robust for machine learning models.
winsorize_var <- function(x) {
  quant <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmax(pmin(x, quant[2]), quant[1])  # Caps values at 1st and 99th percentiles
}

# Apply winsorization separately for each year and month
output_dt_clean <- output_dt_clean %>%
  group_by(year, month) %>%  # Group by year and month
  mutate(across(
    .cols = where(is.numeric) & !any_of("permno"), 
    .fns = winsorize_var
  )) %>%
  ungroup()  # Ungroup after winsorization

#winsorization for some reason creates , value to my permnos, solved it by excluding permnos

#winsorizing separately for each year and month to avoid look ahead bias 


# Step 2 - Log transformations (only for selected variables)
output_dt_clean <- output_dt_clean %>%
  mutate(
    log_bm  = log(pmax(bm, 1e-6)),   # Book-to-market
    log_ep  = log(pmax(ep, 1e-6)),   # Earnings-to-price
    log_dy  = log(pmax(dy, 1e-6)),   # Dividend yield
    log_lev = log(pmax(lev, 1e-6)),  # Leverage
    log_vol = log(pmax(volatility, 1e-6)),   # Volatility
    log_sp  = log(pmax(sp, 1e-6))    # Sales-to-price
    
    # mve is assumed already logged if needed
  )

names(output_dt_clean)

names(output_dt_clean)

# Step 3 - Lag ALL predictor variables appropriately
output_dt_clean <- output_dt_clean %>%
  arrange(permno, year, month) %>%
  group_by(permno) %>%
  mutate(across(
    c(agr, sgr, chinv, pchcapx, grltnoa, pchsaleinv, chcsho,
      mve, log_bm, log_ep, log_dy, log_lev, log_sp,
      cashpr, roic, log_vol),
    ~lag(.x, 1)
  )) %>%
  ungroup()


# Step 4 - Remove rows with missing values caused by lagging
output_dt_clean <- output_dt_clean %>%
  filter(!is.na(mve))  # or use drop_na() to remove all rows with any NA

#every permno has the first ob missing because we lagged

# Step 4.5 - Define final predictor set (logged and raw where appropriate)
# We're using log-transformed vars where needed and raw for the rest
predictor_vars <- c(
  "agr", "sgr", "chinv", "pchcapx", "grltnoa", "pchsaleinv", "chcsho",
  "mve", "log_bm", "log_ep", "log_dy", "log_lev", "log_sp",
  "cashpr", "roic", "log_vol"
)

# Select columns for training and testing sets
train_output_dt <- output_dt_clean %>%
  filter(year > 2000, year <= 2012) %>%
  select(year, month, permno, all_of(predictor_vars), excess_return)

test_output_dt <- output_dt_clean %>%
  filter(year > 2012, year <= 2021) %>%
  select(year, month, permno, all_of(predictor_vars), excess_return)

# Standardize within each month in the full dataset
output_dt_clean <- output_dt_clean %>%
  group_by(year, month) %>%  # Standardize per month
  mutate(across(
    all_of(predictor_vars), 
    ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>%
  ungroup()

summary(output_dt_clean$year)
summary(test_output_dt$year)

# means and standard deviations from training data only
means <- sapply(train_output_dt[predictor_vars], mean, na.rm = TRUE)
sds   <- sapply(train_output_dt[predictor_vars], sd, na.rm = TRUE)

# scaling to training data
train_output_dt[predictor_vars] <- scale(train_output_dt[predictor_vars], center = means, scale = sds)

# Target = excess_ret
# Predictors = the standardized log variables

summary(output_dt) 


#LOGISTIC REGRESSION
output_dt_clean <- output_dt_clean %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))
#1 is above, 0 below

output_dt_clean%>%
  group_by(year, label = ifelse(excessreturn_binary == 'Above', "Above", "Below")) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(year) %>% print(n = Inf)

above_count <- sum(output_dt_clean$excessreturn_binary == "Above")
below_count <- sum(output_dt_clean$excessreturn_binary == "Below")

train_output_dt <- output_dt_clean %>% filter(year <= 2010) %>% select(year, permno, month, agr, sgr, chinv, pchcapx, grltnoa, pchsaleinv, chcsho,
                                                                       mve, log_bm, log_ep, log_dy, log_lev, log_sp,
                                                                       cashpr, roic, excessreturn_binary)
test_output_dt  <- output_dt_clean %>% filter(year > 2010, year <= 2015) %>% select(year, permno, month, agr, sgr, chinv, pchcapx, grltnoa, pchsaleinv, chcsho,
                                                                                    mve, log_bm, log_ep, log_dy, log_lev, log_sp,
                                                                                    cashpr, roic, excessreturn_binary)

describe(output_dt_clean$excess_return)

# histogram 
hist(output_dt_clean$excess_return, 
     main = "Histogram of Excess Return", 
     xlab = "Excess Return", 
     col = "lightblue", 
     border = "black")


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
  coord_polar(theta = "y") +  
  labs(title = "Distribution of Excess Return Categories") +
  theme_void() +  # Removes background grid
  scale_fill_manual(values = c("Above" = "blue", "Below" = "skyblue"))

#clearly there is a big disparity in count between classes so we need to undersample
#the majority, due to the size of the data set we could not oversample to "above" class
#there were too few 

#doing logistic reg on the big data set, first oversampling minority

# OPTION 1: Undersample the majority class
# Find the minority class count
#min_class_count <- min(table(train_output_dt$excessreturn_binary))

#train_output_balanced <- train_output_dt %>%
 # group_by(excessreturn_binary) %>%
  #sample_n(min_class_count) %>%
  #ungroup()

# OPTION 2: Find minority and majority class sizes and oversample minority
minority_class <- train_output_dt %>% filter(excessreturn_binary == "Above")
majority_class <- train_output_dt %>% filter(excessreturn_binary == "Below")

# Oversample minority class by duplicating it
oversampled_minority <- minority_class %>% sample_n(nrow(majority_class), replace = TRUE)

# Combine oversampled minority with original majority
train_output_balanced <- bind_rows(majority_class, oversampled_minority)

# Shuffle data
train_output_balanced <- train_output_balanced %>% sample_frac(1)

# Check new class distribution
table(train_output_balanced$excessreturn_binary)

hist(train_output_balanced$excess_return, 
     main = "Histogram of Excess Return", 
     xlab = "Excess Return", 
     col = "lightblue", 
     border = "black")

##### MODEL ###

train_output_balanced <- train_output_balanced %>% na.omit() %>% select(agr, sgr, chinv, pchcapx, grltnoa, pchsaleinv, chcsho,
                                                                        mve, log_bm, log_ep, log_dy, log_lev, log_sp,
                                                                        cashpr, roic, excessreturn_binary)
test_output_dt <- test_output_dt %>% na.omit() %>% select(agr, sgr, chinv, pchcapx, grltnoa, pchsaleinv, chcsho,
                                                                        mve, log_bm, log_ep, log_dy, log_lev, log_sp,
                                                                        cashpr, roic, excessreturn_binary)

train_output_balanced$excessreturn_binary <- factor(train_output_balanced$excessreturn_binary, levels = c("Below", "Above"))
test_output_dt$excessreturn_binary <- factor(test_output_dt$excessreturn_binary, levels = c("Below", "Above"))
#this line is to run a Logistic Regression model
LRmodel <- glm(excessreturn_binary~ ., family='binomial', train_output_balanced)
#this line is to eliminate irrelevant variables using a stepwise approach
LRmodelR <- step(LRmodel)
probabilitiesLR <- predict(LRmodelR, test_output_dt [,-16],type= "response")
predictionLR <- ifelse(probabilitiesLR > 0.5, " Above", " Below")
#if a predicted prob is higher than 0.6 then its above, if not below
#classificationtable <- table(pred= predictionLR, test_output_dt[,9])
classificationtable <- table(pred= predictionLR, test_output_dt$excessreturn_binary)
acctestLR <- sum(diag(classificationtable))/sum(classificationtable)
acctestLR
print(classificationtable) #works super well :) 

#find optimal threshold

# probabilities from the logistic regression model
probabilitiesLR <- predict(LRmodelR, test_output_dt[, -16], type = "response")

# ROC curve object
roc_curve <- roc(test_output_dt$excessreturn_binary, probabilitiesLR, levels = c("Below", "Above"))

# ROC Curve
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

# AUC and optimal threshold
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))
print(paste("Optimal Classification Threshold:", round(optimal_threshold, 4)))
print(optimal_threshold)

# optimal threshold to make predictions
predictionLR_optimal <- ifelse(probabilitiesLR > optimal_threshold, "Above", "Below")

# Create confusion matrix for new predictions
classificationtable_optimal <- table(Predicted = predictionLR_optimal, Actual = test_output_dt$excessreturn_binary)
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
  labs(title = "Confusion Matrix Heatmap (Percentages) for Logistic Regression", 
       x = "Actual Class", 
       y = "Predicted Class") +
  theme_minimal()

summary(LRmodelR)

#for some reason my model completely predicts above when below
#for imbalanced data this is not good though, lets try random forests

####DONE HERE