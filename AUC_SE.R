npv <- 0.2593 # Negative Predictive Value
ppv <- 0.375 # Positive Predictive Value
sensitivity <- 0.1667 # Sensitivity
specificity <- 0.5122 # Specificity 
auc <- 0.6606 # Area Under the Curve
total_sample_size <- 113 # Total sample size
# Calculating the prevalence
# The above data is based on aSAH database
# to calculate AUC for the data
# I considerd s100b
# auc(roc(as.numeric(factor(aSAH$outcome, levels = c("Poor", "Good"), labels = c(0, 1))), as.numeric(factor(aSAH$s100b>0.3, levels = c(FALSE, TRUE), labels = c(0, 1)))))

a <- sensitivity
b <- 1 - specificity
c <- a / ppv

prevalence <- b / (c - a + b)

# Estimating positive and negative cases 
positive_cases <- round(prevalence * total_sample_size)
negative_cases <- round(total_sample_size - positive_cases)

# Calculate Q1 and Q2
Q1 <- auc / (2 - auc)
Q2 <- 2 * auc^2 / (1 + auc)

# Calculate the standard error of the AUC
SE <- sqrt((auc * (1 - auc) + (positive_cases - 1) * (Q1 - auc^2) + (negative_cases - 1) * (Q2 - auc^2)) / (positive_cases * negative_cases))

# Calculating 95% CI: AUC ± 1.96 * SE 8
CI_lower <- auc - 1.96 * SE
CI_upper <- auc + 1.96 * SE

# Output
list(prevalence = prevalence, positive_cases = positive_cases, negative_cases = negative_cases, SE = SE, CI_lower = CI_lower, CI_upper = CI_upper)
