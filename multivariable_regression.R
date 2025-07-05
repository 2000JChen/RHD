# Script to run a multivariable linear regression on death counts
# and create a forest plot of predictor coefficients.

# Required packages
# install.packages(c("readxl", "dplyr", "broom", "ggplot2"))

library(readxl)
library(dplyr)
library(broom)
library(ggplot2)

# Path to the Excel file
excel_file <- "Underlying Cause of Death, 1999-2020.xlsx"

# Read the first sheet
# This assumes the sheet uses the columns:
# Race, 2013 Urbanization, Census Division, Sex, Sex Code, Year, Deaths, Population
mortality <- read_excel(excel_file)

# Fit linear model: deaths as a function of year and demographic predictors
model <- lm(Deaths ~ Year + Race + `2013 Urbanization` + `Census Division` + Sex,
            data = mortality)

# Summarize model
summary(model)

# Prepare results for forest plot
coefs <- tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = factor(term, levels = term))

# Forest plot
ggplot(coefs, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = "Predictor", y = "Coefficient",
       title = "Multivariable Linear Regression for Deaths per Year") +
  theme_minimal()
