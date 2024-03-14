library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(lme4, quietly=TRUE, verbose=FALSE)
library(lmerTest, quietly=TRUE, verbose=FALSE)
library(performance, quietly=TRUE, verbose=FALSE)
library(car)
library(mice)
library(leaps)
library(MASS)
library(pROC)
#read in data
PCE1 <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/Percent Change From Preceding Period in Real Personal Consumption Expenditures by Type of Product.csv', header = TRUE)

dim(PCE1)
head(PCE1)
colnames(PCE1)
summary(PCE1)
sum(is.na(PCE1))
view(PCE1)

colnames(PCE1)[-1] <- paste(PCE1[1, -1], colnames(PCE1)[-1])
PCE1 <- PCE1[-1, ] 

PCE1$X

count_leading_spaces <- function(data, column_name) {
  data %>%
    mutate(LeadingSpaces = str_extract(get(column_name), "^\\s*") %>% nchar())
}

PCE1 <- count_leading_spaces(PCE1, 'X')

PCE_products1 <- PCE1 %>%
  filter(LeadingSpaces == 0)

PCE_products1$LeadingSpaces <-NULL

PCE_long1 <- pivot_longer(PCE_products1,
                         cols = -`X`,
                         names_to = c("Quarter", "Year"),
                         names_sep = " ",
                         values_to = "PCE")


current_categories <- rep(NA,5)
transformed_data <- data.frame()

PCE1$LeadingSpaces

for(i in 1:nrow(PCE1)) {
  row <- PCE1[i, ]
  
  # Determine the category level based on LeadingSpaces
  if(row$LeadingSpaces == 0) { # This row is a product
    # Prepare a new row for the transformed_data
    new_row <- data.frame(ProductName = row$X, Level1 = current_categories[1], Level2 = current_categories[2],
                          Level3 = current_categories[3], Level4 = current_categories[4], Level5 = current_categories[5])
    # Add the new_row to the transformed_data
    transformed_data <- rbind(transformed_data, new_row)
  } else {
    # This row is a category, update the current_categories based on LeadingSpaces
    level <- which(c(2, 4, 8, 12, 16) == row$LeadingSpaces)
    if(length(level) > 0) {
      current_categories[level] <- row$X
      # Reset deeper categories
      if(level < length(current_categories)) {
        current_categories[(level+1):length(current_categories)] <- NA
      }
    }
  }
}

merged_PCE1 <- merge(PCE_long1, transformed_data, by.x = "X", by.y = "ProductName")

merged_PCE1$Year <- as.character(merged_PCE1$Year) # Convert to character in case it's not

# Remove all non-digit characters, including "X" and anything after the dot
merged_PCE1$Year <- sub("X", "", merged_PCE1$Year) 
merged_PCE1$Year <- sub("\\..*", "", merged_PCE1$Year)

#convert back to numeric
merged_PCE1$Year <- as.numeric(merged_PCE1$Year)
merged_PCE1 <- merged_PCE1[!is.na(merged_PCE1$PCE), ]

UNRATE <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/Unemployment_Rate.csv', header = TRUE)
REINTERATE <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/10Year_Real_Interest_Rate.csv', header = TRUE)
CONSENTI <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/UMich_Consumer_Sentiment.csv', header = TRUE)
INFLATRATE <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/10Year_Breakeven_Inflation_Rate.csv', header = TRUE)
REDISPINC <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/Real_Disposable_Personal_Income.csv', header = TRUE)
HPIPO <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/House_Price_Index_Purchase_Only.csv', header = TRUE)
REGDPCAP <- read.csv('/Users/jiayiwu/Desktop/DS Capstone/Data/Real gross domestic product per capita.csv', header = TRUE)

# Create a year-quarter key
UNRATE <- UNRATE %>%
  mutate(
    year = year(DATE),
    quarter = paste0("Q", quarter(DATE)),
    year_quarter = paste(year, quarter, sep = "-")
  )

UNRATE$DATE <- NULL
UNRATE$year <- NULL
UNRATE$quarter <- NULL

REINTERATE <- REINTERATE %>%
  mutate(
    year = year(DATE),
    quarter = paste0("Q", quarter(DATE)),
    year_quarter = paste(year, quarter, sep = "-")
  )

REINTERATE$DATE <- NULL
REINTERATE$year <- NULL
REINTERATE$quarter <- NULL
names(REINTERATE)[names(REINTERATE) == "REAINTRATREARAT10Y"] <- "REINTERATE"

CONSENTI <- CONSENTI %>%
  mutate(
    year = year(DATE),
    quarter = paste0("Q", quarter(DATE)),
    year_quarter = paste(year, quarter, sep = "-")
  )

CONSENTI$DATE <- NULL
CONSENTI$year <- NULL
CONSENTI$quarter <- NULL
names(CONSENTI)[names(CONSENTI) == "UMCSENT"] <- "CONSENTI"

INFLATRATE <- INFLATRATE %>%
  mutate(
    year = year(DATE),
    quarter = paste0("Q", quarter(DATE)),
    year_quarter = paste(year, quarter, sep = "-")
  )

INFLATRATE$DATE <- NULL
INFLATRATE$year <- NULL
INFLATRATE$quarter <- NULL
names(INFLATRATE)[names(INFLATRATE) == "T10YIE"] <- "INFLATRATE"

REDISPINC <- REDISPINC %>%
  mutate(
    year = year(DATE),
    quarter = paste0("Q", quarter(DATE)),
    year_quarter = paste(year, quarter, sep = "-")
  )

REDISPINC$DATE <- NULL
REDISPINC$year <- NULL
REDISPINC$quarter <- NULL
names(REDISPINC)[names(REDISPINC) == "DSPIC96"] <- "REDISPINC"

HPIPO <- HPIPO %>%
  mutate(
    year_quarter = paste(Year, Quarter, sep = "-")
  )

HPIPO$Year <- NULL
HPIPO$Quarter <- NULL
names(HPIPO)[names(HPIPO) == "Seasonally.Adjusted.Purchase.Only.Index...1991Q1.100."] <- "SAPOI1991Q1100"
names(HPIPO)[names(HPIPO) == "Seasonally.Adjusted.Purchase.Only.Index...Change.Over..Previous.Quarter"] <- "SAPOIPERCHG1991Q1100"

REGDPCAP <- REGDPCAP %>%
  mutate(
    year = year(DATE),
    quarter = paste0("Q", quarter(DATE)),
    year_quarter = paste(year, quarter, sep = "-")
  )

REGDPCAP$DATE <- NULL
REGDPCAP$year <- NULL
REGDPCAP$quarter <- NULL
names(REGDPCAP)[names(REGDPCAP) == "A939RX0Q048SBEA"] <- "REGDPCAP"

merged_PCE1 <- merged_PCE1 %>%
  mutate(year_quarter = paste(Year, Quarter, sep = "-"))

# Merge the datasets based on the year-quarter key
merged_PCE1 <- left_join(UNRATE, merged_PCE1, by = "year_quarter")
merged_PCE1 <- left_join(REINTERATE, merged_PCE1, by = "year_quarter")
merged_PCE1 <- left_join(CONSENTI, merged_PCE1, by = "year_quarter")
merged_PCE1 <- left_join(INFLATRATE, merged_PCE1, by = "year_quarter")
merged_PCE1 <- left_join(REDISPINC, merged_PCE1, by = "year_quarter")
merged_PCE1 <- left_join(HPIPO, merged_PCE1, by = "year_quarter")
merged_PCE1 <- left_join(REGDPCAP, merged_PCE1, by = "year_quarter")

#merged_PCE1$year_quarter <- NULL
cols_to_move <- c("UNRATE", "REINTERATE", "CONSENTI", "INFLATRATE", "REDISPINC", "SAPOI1991Q1100", "SAPOIPERCHG1991Q1100", "REGDPCAP")
other_cols <- setdiff(names(merged_PCE1), cols_to_move)

# Combine the other columns with the ones you want to move to the end
new_order <- c(other_cols, cols_to_move)

# Reorder the dataframe columns
merged_PCE1 <- merged_PCE1[, new_order]

merged_PCE1$PCE <-as.numeric(merged_PCE1$PCE)
merged_PCE1$Year <- as.numeric(merged_PCE1$Year)
merged_PCE1$Quarter <- as.numeric(sub("Q", "", merged_PCE1$Quarter))
merged_PCE1$year_quarter <- NULL
merged_PCE1$SAPOIPERCHG1991Q1100 <- as.numeric(gsub("%", "", merged_PCE1$SAPOIPERCHG1991Q1100))

merged_PCE1 <- merged_PCE1[order(merged_PCE1$X),]
merged_PCE_sample <- merged_PCE1[1:60,]
write.csv(merged_PCE_sample,file = "PCEsample.csv", row.names = FALSE)

summary(merged_PCE1)
merged_PCE1 <- merged_PCE1[!is.na(merged_PCE1$PCE), ]

model0 <- lmer(PCE ~ Year + Quarter + INFLATRATE + CONSENTI + REINTERATE + UNRATE + REDISPINC + SAPOI1991Q1100 + SAPOIPERCHG1991Q1100 + (1|Level1) + (1|Level1:Level2), data = merged_PCE1)

summary(model0)
plot(model0)
vif(model0)
r2(model0)
model_performance(model0)

merged_PCE1$PCE_log <- log(merged_PCE1$PCE + 0.1)
sum(is.na(merged_PCE1$PCE_log))
sum(is.infinite(merged_PCE1$PCE_log))
merged_PCE_log <- merged_PCE1[!is.na(merged_PCE1$PCE_log) & !is.infinite(merged_PCE1$PCE_log), ]
# Standardize continuous predictors
predictors <- c("INFLATRATE", "CONSENTI", "REINTERATE", "UNRATE", "REDISPINC", "SAPOI1991Q1100", "SAPOIPERCHG1991Q1100")
merged_PCE_log[predictors] <- scale(merged_PCE_log[predictors])
# Fit the model after transformations and check for any remaining issues
model_log <- lmer(PCE_log ~ Year + Quarter + INFLATRATE + CONSENTI + REINTERATE + UNRATE + REDISPINC + SAPOI1991Q1100 + SAPOIPERCHG1991Q1100 + (1|Level1) + (1|Level1:Level2), data = merged_PCE_log)
summary(model_log)
vif(model_log)
r2(model_log)


# Stepwise model selection based on AIC
stepwise_model <- step(model_log, direction="both")
stepwise_model

final_model <- lmer(PCE_log ~ Year + Quarter + INFLATRATE + CONSENTI + REINTERATE + UNRATE + REDISPINC + SAPOI1991Q1100 + (1 | Level1:Level2), data = merged_PCE_log)
model_performance(final_model)
summary(final_model)
plot(final_model)


# Check residuals vs fitted values
plot(resid(final_model) ~ fitted(final_model))
abline(h = 0, col = "red")

# Check normality of residuals with a Q-Q plot
qqnorm(resid(final_model))
qqline(resid(final_model))

# Check for homoscedasticity (constant variance of residuals)
plot(scale(resid(final_model)) ~ fitted(final_model))

# Leverage plot to identify influential cases
influencePlot(final_model, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")

# Calculate Cook's distance for the final_model
cooks_d <- cooks.distance(final_model)

# Define a threshold for influential observations as 4/(n-k)
n <- nrow(merged_PCE1)  # Total number of observations
k <- length(fixef(final_model))  # Number of fixed effects in the model
threshold <- 4 / (n - k)

# Identify the indices of influential observations based on Cook's distance
influential_obs <- which(cooks_d > threshold)
influential_obs

# Consider plotting the standardized residuals to find outliers
stdres <- residuals(final_model, type = "pearson")
plot(stdres, type = "p")
abline(h = c(-3, 3), col = "red")

# Identify observations with large standardized residuals
large_resid_indices <- which(abs(stdres) > 3)
large_resid_indices
outliers <- intersect(large_resid_indices, influential_obs)
outliers

merged_PCE_clean <- merged_PCE_log[-outliers, ]
final_model2 <- lmer(PCE_log ~ Year + Quarter + INFLATRATE + CONSENTI + REINTERATE + UNRATE + REDISPINC + SAPOI1991Q1100 + (1|Level1), data = merged_PCE_clean)
model_performance(final_model2)
summary(final_model2)
plot(final_model2)


# Model 0: Residuals vs Fitted
plot(resid(model0) ~ fitted(model0), main="Model 0: Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

# Final Model: Residuals vs Fitted
plot(resid(final_model) ~ fitted(final_model), main="Final Model: Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

# Final Model 2: Residuals vs Fitted
plot(resid(final_model2) ~ fitted(final_model2), main="Final Model 2: Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

# Check normality of residuals with a Q-Q plot
qqnorm(resid(final_model2))
qqline(resid(final_model2))


