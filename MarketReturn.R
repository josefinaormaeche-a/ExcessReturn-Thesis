#installing packages
install.packages("writexl")
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
install.packages('rpart')
install.packages("rpart.plot")
install.packages("ssh")
install.packages("ranger")
install.packages("keras")

#needed libraries
library(writexl)
library(ranger)
library(ssh)
library(rpart)
library(rpart.plot)
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
library(tidyverse)
library(tseries)
library(TTR)


file_path <- "C:/Users/joseo/OneDrive/Desktop/cbs/Fourth Semester/output22.csv"
file_path4 <- "C:/Users/joseo/OneDrive/Desktop/cbs/Fourth Semester/sp500.csv"

#output <- read.xlsx(file_path)
output <- read.csv2(file_path)
sp500 <- read.csv(file_path4)

sp500 <- sp500 %>%
  mutate(caldt = as.Date(caldt)) %>%
  filter(year(caldt) >= 1999 & year(caldt) <= 2022) %>%
  filter(!is.na(sprtrn)) %>%  
  group_by(year = year(caldt), month = month(caldt)) %>%
  arrange(caldt) 

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

# Filter dataset based on occurrence count, only permnos that appear more than 50 times
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

#stock_data <- f_output %>% filter(permno %in% c(23297, 10325,82699))
#this is going to be our reduced data set

#descriptive statistics
summary(output)

names(output)

#Merging RF to general dataset
sp500 <- sp500 %>%
  mutate(
    year = year(caldt),
    month = month(caldt)
  ) %>%
  select(year, month, sprtrn) %>%
  distinct(year, month, .keep_all = TRUE) %>%
  arrange(year, month)

# Convert both datasets to data.table
sp500_dt <- as.data.table(sp500)
output_dt_sp <- as.data.table(f_output)

# Set keys for joining
setkey(sp500_dt, year, month)
setkey(output_dt_sp, fyear, month_id)

# Rolling join: sp500_dt will join with f_output based on year and month
output_dt_sp <- sp500_dt[output_dt_sp, roll = TRUE]

# Calculate market return and excess return
output_dt_sp <- output_dt_sp %>%
  mutate(
    market_monthly = sprtrn,  # Monthly S&P 500 return
    RET_adj = ifelse(!is.na(DLRET), (1 + RET) * (1 + DLRET) - 1, RET),
    excess_return = RET_adj - market_monthly
  ) %>%
  relocate(market_monthly, .before = sprtrn) %>%
  relocate(excess_return, .before = RET_adj)

# Identify and remove any permnos with invalid excess returns
permnos_to_remove <- output_dt_sp %>%
  group_by(permno) %>%
  filter(any(is.na(excess_return) | !is.finite(excess_return))) %>%
  pull(permno) %>%
  unique()

# Filter out bad permnos
output_dt_market_clean <- output_dt_sp %>%
  filter(!permno %in% permnos_to_remove)


output_dt_market_clean <-output_dt_market_clean %>% arrange(permno, year,month)

# 12-month rolling standard deviation (volatility)
output_dt_market_clean <- output_dt_market_clean %>%
  arrange(permno, year, month) %>%
  group_by(permno) %>%
  mutate(volatility = if (n() >= 12) runSD(RET, n = 12) else NA_real_) %>%
  ungroup()

#mometum
output_dt_market_clean <- output_dt_market_clean %>%
  arrange(permno, year, month) %>%  # Sort by permno and time
  group_by(permno) %>%
  mutate(momentum_12m = rollapply(
    RET, 12, function(x) prod(1 + x) - 1, align = "right", fill = NA
  )) %>%
  ungroup()

# Done!


predictorss <- output_dt_market_clean  %>% filter(year>=2001)%>% select(year, month, permno, roe, agr, sgr, momentum_12m, chinv, pchcapx, grltnoa, pchsaleinv, chcsho, mve, bm, ep, cashpr, dy, lev, sp, roic, volatility, excess_return)
#in order to decide whether i will log the predictor variables, i will perform an ADF
#test to see if they are or not stationary

agr_no_na <- na.omit(output_dt_market_clean$agr)
bm_no_na <- na.omit(output_dt_market_clean$bm)
agr1_no_na <- na.omit(output_dt_market_clean$sgr)
agr2_no_na <- na.omit(output_dt_market_clean$chinv)
agr3_no_na <- na.omit(output_dt_market_clean$pchcapx)
agr4_no_na <- na.omit(output_dt_market_clean$grltnoa)
agr5_no_na <- na.omit(output_dt_market_clean$pchsaleinv)
agr6_no_na <- na.omit(output_dt_market_clean$chcsho)
agr7_no_na <- na.omit(output_dt_market_clean$mve)
agr9_no_na <- na.omit(output_dt_market_clean$ep)
agr10_no_na <- na.omit(output_dt_market_clean$cashpr)
agr11_no_na <- na.omit(output_dt_market_clean$dy)
agr12_no_na <- na.omit(output_dt_market_clean$lev)
agr13_no_na <- na.omit(output_dt_market_clean$sp)
agr14_no_na <- na.omit(output_dt_market_clean$roic)
agr15_no_na <- na.omit(output_dt_market_clean$volatility)
agr16_no_na <- na.omit(output_dt_market_clean$momentum_12m)
agr17_no_na <- na.omit(output_dt_market_clean$roe)
agr18_no_na <- na.omit(output_dt_market_clean$acc)

adf_test_result <- adf.test(agr_no_na, alternative = "stationary")
print(adf_test_result)
#below 0,05 i reject the null, accept the alternative

adf_test_result_bm <- adf.test(bm_no_na, alternative = "stationary")
print(adf_test_result_bm)
#bm are stationary
adf_test_result1 <- adf.test(agr1_no_na, alternative = "stationary")
print(adf_test_result1)
#sgr are stationary
adf_test_result2 <- adf.test(agr2_no_na, alternative = "stationary")
print(adf_test_result2)
#chinv are stationary
adf_test_result3 <- adf.test(agr3_no_na, alternative = "stationary")
print(adf_test_result3)
#phcapx are stationary
adf_test_result4 <- adf.test(agr4_no_na, alternative = "stationary")
print(adf_test_result4)
#grltnoa is stationary
adf_test_result5 <- adf.test(agr5_no_na, alternative = "stationary")
print(adf_test_result5)
#pchsaleinv is stationary
adf_test_result6 <- adf.test(agr6_no_na, alternative = "stationary")
print(adf_test_result6)
#chcsho is stationary
adf_test_result7 <- adf.test(agr7_no_na, alternative = "stationary")
print(adf_test_result7)
#mve is stationary
adf_test_result9 <- adf.test(agr9_no_na, alternative = "stationary")
print(adf_test_result9)
#ep is stationary
adf_test_result10 <- adf.test(agr10_no_na, alternative = "stationary")
print(adf_test_result10)
#cashpr is stationary
adf_test_result11 <- adf.test(agr11_no_na, alternative = "stationary")
print(adf_test_result11)
#dy is stationary
adf_test_result12 <- adf.test(agr12_no_na, alternative = "stationary")
print(adf_test_result12)
#lev is stationary
adf_test_result13 <- adf.test(agr13_no_na, alternative = "stationary")
print(adf_test_result13)
#sp is stationary
adf_test_result14 <- adf.test(agr14_no_na, alternative = "stationary")
print(adf_test_result14)
#roic is stationary
adf_test_result15 <- adf.test(agr15_no_na, alternative = "stationary")
print(adf_test_result15)
#volatility is stationry
adf_test_result16 <- adf.test(agr16_no_na, alternative = "stationary")
print(adf_test_result16)
#mometum is stationary
adf_test_result17 <- adf.test(agr17_no_na, alternative = "stationary")
print(adf_test_result17)
#roe is stationary
adf_test_result18 <- adf.test(agr18_no_na, alternative = "stationary")
print(adf_test_result18)
#accruals is stationary
#they are all stationary, no unit root, no log normalization needed

### done with merging

num_unique_permno_clean <- output_dt_market_clean %>% distinct(permno) %>% nrow()
print(num_unique_permno_clean)
#6321 stocks total

#SUCCESFULLY FILTERED WUHU

# bubble chart for data visualization
# Filter the dataset to include only records where the fiscal year is 2020,
#could also be done on a permno base
filtered_year <- output_dt_sp%>%
  filter(year == 2016)  

#i basically want to create a monthly column here so that for any year if gives 1 to the first 
#occurance of that year and 12 to the last, fitting all the ones in between in order as well


# Create the bubble chart with ggplot2
p <- ggplot(filtered_year, aes(x = permno, y = excess_return, size = abs(excess_return), color = excess_return)) +  # Color by RET
  geom_point(alpha = 0.6) +  # 'alpha' controls transparency
  scale_size_continuous(name = "Absolute RET", range = c(2, 10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Bubble Chart of RET for Fiscal Year 2016",
       x = "permno",
       y = "RET") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

# Convert ggplot object to a plotly interactive plot
interactive_plot <- ggplotly(p, tooltip = "permno")

# Display the plot
interactive_plot

#averaging out volatility per year across permnos
output_dt_market_clean %>%
  filter(is.finite(volatility)) %>%
  group_by(year, month) %>%
  summarise(mean_volatility = mean(volatility), .groups = "drop") %>%
  ggplot(aes(x = as.Date(paste(year, month, "01", sep = "-")), y = mean_volatility)) +
  geom_line(color = "#4C72B0", size = 1) +
  labs(
    title = "Average 12-Month Rolling Volatility Over Time",
    x = "Date",
    y = "Average Volatility"
  ) +
  theme_minimal()

## PREPARING THE DATA SET FOR MODELS

# Filter stocks where DLRET ≠ 0 and RET ≠ DLRET
different_delist_stocks <- output_dt_sp%>%
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
output_dt_market_clean <- output_dt_market_clean %>%
  group_by(year, month) %>%  
  mutate(across(
    .cols = where(is.numeric) & !any_of("permno"), 
    .fns = winsorize_var
  )) %>%
  ungroup()  # Ungroup after winsorization

#winsorization for some reason creates , value to my permnos, solved it by excluding permnos

#winsorizing separately for each year and month to avoid look ahead bias 


# Step 2 - Log transformations (only for selected variables)
output_dt_market_clean <- output_dt_market_clean %>%
  mutate(
    log_bm  = log(pmax(bm, 1e-6)),   # Book-to-market
    log_ep  = log(pmax(ep, 1e-6)),   # Earnings-to-price
    log_dy  = log(pmax(dy, 1e-6)),   # Dividend yield
    log_lev = log(pmax(lev, 1e-6)),  # Leverage
    log_vol = log(pmax(volatility, 1e-6)),   # Volatility
    log_sp  = log(pmax(sp, 1e-6))    # Sales-to-price
    
    # mve is assumed already logged if needed
  )

names(output_dt_market_clean)

#should bm be replaced by log_bm?

# Step 3 - Lag ALL predictor variables appropriately
output_dt_market_clean <- output_dt_market_clean %>%
  arrange(permno, year, month) %>%
  group_by(permno) %>%
  mutate(across(
    c(agr, sgr, chinv, pchcapx, grltnoa, pchsaleinv, chcsho,
      mve, log_bm, log_ep, log_dy, log_lev, log_sp,
      cashpr, roic, log_vol, momentum_12m, roe),
    ~lag(.x, 1)
  )) %>%
  ungroup()


# Step 4 - Remove rows with missing values caused by lagging
output_dt_market_clean <- output_dt_market_clean %>%
  filter(!is.na(mve))  # or use drop_na() to remove all rows with any NA

#every permno has the first ob missing because we lagged

# Step 4.5 - Define final predictor set (logged and raw where appropriate)
# We're using log-transformed vars where needed and raw for the rest
predictor_vars <- c(
  "agr", "sgr", "chinv", "pchcapx", "grltnoa", "pchsaleinv", "chcsho",
  "mve", "log_bm", "log_ep", "log_dy", "log_lev", "log_sp",
  "cashpr", "roic", "log_vol", "momentum_12m", "roe"
)

# Select columns for training and testing sets
train_output_dt <- output_dt_market_clean %>%
  filter(year > 2000, year <= 2012) %>%
  select(year, month, permno, all_of(predictor_vars), excess_return)

test_output_dt <- output_dt_market_clean %>%
  filter(year > 2012, year <= 2021) %>%
  select(year, month, permno, all_of(predictor_vars), excess_return)

#making sure to respect order of time for how the model runs and use smaller models for testing first

# Standardize within each month in the full dataset
output_dt_market_clean <- output_dt_market_clean %>%
  group_by(year, month) %>%  # Standardize per month
  mutate(across(
    all_of(predictor_vars), 
    ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>%
  ungroup()

summary(output_dt_market_clean$year)
summary(test_output_dt$year)

# means and standard deviations from training data only
means <- sapply(train_output_dt[predictor_vars], mean, na.rm = TRUE)
sds   <- sapply(train_output_dt[predictor_vars], sd, na.rm = TRUE)

# scaling to training data
train_output_dt[predictor_vars] <- scale(train_output_dt[predictor_vars], center = means, scale = sds)

# Target = excess_ret
# Predictors = the standardized log variables

#distribution of log volatility
output_dt_market_clean %>%
  filter(is.finite(log_vol)) %>%  # Remove any remaining NA/Inf
  ggplot(aes(x = log_vol)) +
  geom_histogram(binwidth = 0.05, fill = "#4C72B0", color = "white", alpha = 0.9) +
  labs(
    title = "Distribution of Log(Volatility)",
    x = "Log Volatility",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)


#evaluating all the predictor variables
predictor_data <- output_dt_market_clean %>%
  select(all_of(predictor_vars)) %>%
  filter(if_all(everything(), ~ is.finite(.)))  # Drop any rows with NA/Inf

predictor_data_long <- predictor_data %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(predictor_data_long, aes(x = value)) +
  geom_histogram(bins = 40, fill = "#4C72B0", color = "white", alpha = 0.9) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  labs(
    title = "Distribution of Predictor Variables",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)


#Investigating my returns

describe(train_output_dt$excess_return)

# histogram 
hist(train_output_dt$excess_return, 
     main = "Histogram of Excess Return", 
     xlab = "Excess Return", 
     col = "lightblue", 
     border = "black")

#variables tally up to the general train_output_dt

#LOGISTIC REGRESSION
output_dt_market_clean <- output_dt_market_clean %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))

train_output_dt <- train_output_dt %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))


test_output_dt <- test_output_dt %>%
  mutate(excessreturn_binary = ifelse(excess_return > 0, "Above", "Below"))

#1 is above, 0 below

output_dt_market_clean%>%
  group_by(year, label = ifelse(excessreturn_binary == 'Above', "Above", "Below")) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(year) %>% print(n = Inf)

above_count <- sum(output_dt_market_clean$excessreturn_binary == "Above")
below_count <- sum(output_dt_market_clean$excessreturn_binary == "Below")


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

#there is a LOT less class disparity here


##### MODEL ###

train_output_dt$excessreturn_binary <- factor(train_output_dt$excessreturn_binary, levels = c("Below", "Above"))
test_output_dt$excessreturn_binary <- factor(test_output_dt$excessreturn_binary, levels = c("Below", "Above"))
#this line is to run a Logistic Regression model
train_output_dt <- train_output_dt %>%
  select(excessreturn_binary, all_of(predictor_vars)) %>%
  na.omit()
test_output_dt <- test_output_dt %>%
  select(excessreturn_binary, all_of(predictor_vars)) %>%
  na.omit()
LRmodel <- glm(excessreturn_binary~ ., family='binomial', train_output_dt)
#this line is to eliminate irrelevant variables using a stepwise approach
LRmodelR <- step(LRmodel)
probabilitiesLR <- predict(LRmodelR, test_output_dt [,-1],type= "response")
predictionLR <- ifelse(probabilitiesLR > 0.5, " Above", " Below")
#if a predicted prob is higher than 0.6 then its above, if not below
#classificationtable <- table(pred= predictionLR, test_output_dt[,9])
classificationtable <- table(Predicted = predictionLR, Actual = test_output_dt$excessreturn_binary)

acctestLR <- sum(diag(classificationtable))/sum(classificationtable)
acctestLR
print(classificationtable) #works super well :) 

#find optimal threshold

# Compute probabilities from the logistic regression model
probabilitiesLR <- predict(LRmodelR,  test_output_dt[, !names(test_output_dt) %in% "excessreturn_binary"], type = "response")

# Create the ROC curve object
roc_curve <- roc(test_output_dt$excessreturn_binary, probabilitiesLR, levels = c("Below", "Above"))

# Plot the ROC Curve
ggplot(data = data.frame(FPR = 1 - roc_curve$specificities, TPR = roc_curve$sensitivities),
       aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "grey") +
  labs(title = "ROC Curve for Logistic Regression",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

# Optimal threshold using Youden's J statistic
optimal_index <- which.max(roc_curve$sensitivities + roc_curve$specificities - 1)
optimal_threshold <- roc_curve$thresholds[optimal_index]

# AUC and optimal threshold
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))
print(paste("Optimal Classification Threshold:", round(optimal_threshold, 4)))
print(optimal_threshold)

# Optimal threshold to make predictions
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

# Save probabilities
predictions_LR <- data.frame(
  actual = test_output_dt$excessreturn_binary,
  predicted_prob = probabilitiesLR,
  predicted_class = predictionLR_optimal  # based on optimal threshold
)

#for some reason my model completely predicts above when below
#51,3% accuracy 

####DONE HERE

#for saving
write_xlsx(train_output_dt, "train_output_dt_18pred.xlsx")
write_xlsx(test_output_dt, "test_output_dt_18pred.xlsx")