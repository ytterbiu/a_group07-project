# ==============================================================================
# SMM047 Probability and Mathematical Statistics (Subject CS1)
# Group Coursework 2025-26
# Group:        Group 07
# Authors (in alphabetical order):
#   - Abdulrahman Alolyan
#   - Benjamin Evans
#   - Amogh Sharma
#   - Nivetha Subbiah
# Professor:    Dr Russell Gerrard
# Institution:  Bayes Business School - City St George's, University of London
# Date:         TBC
# Description:  TBC
# Dependencies:
#   - TBC
#
# ==============================================================================

# Clean environment
rm(list = ls()) # Remove all objects
graphics.off() # Close all graphical devices
cat("\014") # Clean console

# Set working directory (Modify accordingly)
setwd(
  "/Users/eddy/Documents/Actuary MSc/m_SMM047 Probability and Mathematical Statistics (Subject CS1)/juypter_notebooks"
)


#---------- Preliminary processing -----------#

library(quantmod)

# Download the data
getSymbols("^MERV", from = "2018-01-01", to = "2024-12-31")

MERV_Adj <- na.omit(Ad(MERV))
plot(MERV_Adj)

# Calculate the
z_n = na.omit(log(MERV_Adj / dplyr::lag(MERV_Adj)))
plot(z_n)

#---------- Element 1: data cleaning and standard test of normality -----------#

library(summarytools)

summary_table <- descr(
  z_n,
  stats = c("mean", "sd", "skewness", "kurtosis"),
  transpose = TRUE,
  headings = FALSE,
  round.digits = 6
)
print(summary_table)


boxplot(z_n)


# # COVID: 10/04/2020-19/05/2020
#
MERV_Adj <- MERV_Adj[
  !((index(MERV_Adj) >= "2020-04-10") &
    (index(MERV_Adj) <= "2020-05-19"))
]
#
#
z_n = na.omit(log(MERV_Adj / dplyr::lag(MERV_Adj)))

plot(z_n)
boxplot(z_n)

###############################################
############ Goodness-of-fit tests ############
###############################################

# KS: center of the distribution
# CvM: overall fit across the entire range
# Anderson–Darling distance places more weight on observations in the tails of the distribution

library(MASS)
fit_n <- fitdistr(z_n, "normal")

mu <- fit_n$estimate[1]
sigma <- fit_n$estimate[2]
# CDF of the fitted distribution
cdfSPnorm = function(y) {
  return(pnorm(y, mu, sigma))
}

ks.test(z_n, cdfSPnorm)

cdfSPnorm(0)


ks.test(z_n, "pnorm", mean = mu, sd = sigma)

ks.test((z_n - mu) / sigma, "pnorm", mean = 0, sd = 1)
ks.test(z_n, "pnorm", mean = mean(z_n), sd = sd(z_n))

# so not normal

# ks is looking more at the middle whereas the other one looks more at the tails*

#---------- Element 2: investigation of normality by resampling -----------#

# (a) Simulate a sample of size 15 from a Normal distribution with mean and variance equal to
# the sample mean and variance you have just calculated.

# (b)  Use resampling to obtain a sample of size 15 from your z-data.
# Calculate the sample excess kurtosis from this bootstrap sample.
# Repeat this until you have generated 50,000 values of \hat{γ}_2.

#---------- Element 3: investigation of constant mean -----------#

#############
## BOXPLOT ##
#############
