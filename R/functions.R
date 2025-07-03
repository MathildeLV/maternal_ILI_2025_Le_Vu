
# Table for estimates and conf interval (and pvalue)
## Continuous outcomes:
estimates_cont <- function(model)
{beta <- coef(model)
Vb <- vcov(model, unconditional = TRUE)
se <- sqrt(diag(Vb))
pvalue <- summary(model)$coefficients[,4] 
my.ci <- data.frame(cbind(beta, lci = beta-1.96*se, uci = beta + 1.96*se, pvalue))
}
## Continuous outcomes: RLM
estimates_cont_rlm <- function(model)
{beta <- coef(model)
Vb <- vcov(model, unconditional = TRUE)
se <- sqrt(diag(Vb))
my.ci <- data.frame(cbind(beta, lci = beta-1.96*se, uci = beta + 1.96*se))
}

## Binary outcomes: 
estimates_binary <- function(model)
{OR <- exp(coef(model))
beta <- coef(model)
Vb <- vcov(model, unconditional = TRUE)
se <- sqrt(diag(Vb))
lci <- exp(beta-1.96*se)
uci <- exp(beta+1.96*se)
pvalue <- summary(model)$coefficients[, "Pr(>|z|)"]
my.ci <- data.frame(cbind(OR, lci, uci,pvalue))
}

# Function to calculate the mode of a variable
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# function to have a label every second measure of a discrete var
label_every_second <- function(labels) {
  labels[seq(1, length(labels), by = 2)]
}


# Normalizing the values in m to the range [0, 1] using min-max scaling.
normalit <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  (x - min_val) / (max_val - min_val)
}


# Plot for having two different y axis
# Function to scale secondary axis
scale_function <- function(x, scale, shift){
  return (x - shift) / scale
}

# Function to scale secondary variable values
inv_scale_function <- function(x, scale, shift){
  return (x * scale + shift)
}


# get marginally adjmean and CI from rlm
## flu vs no flu

# 
# mmeans_ci_horiz_bin <- function(GGEMMEANS)
# {mean_no <- 100*GGEMMEANS$predicted[1]
# lci_no <- 100*GGEMMEANS$conf.low[1]
# uci_no <- 100*GGEMMEANS$conf.high[1]
# mean_yes <- 100*GGEMMEANS$predicted[2]
# lci_yes <- 100*GGEMMEANS$conf.low[2]
# uci_yes <- 100*GGEMMEANS$conf.high[2]
# d <- 100*(GGEMMEANS$predicted[2] - GGEMMEANS$predicted[1])
# se_diff <-  sqrt(GGEMMEANS$std.error[1]^2 + GGEMMEANS$std.error[2]^2)
# lower_diff <- (d -1.96*se_diff)
# upper_diff <- (d +1.96*se_diff)
# my.ci <- data.frame(
#   mean_no, lci_no, uci_no,
#   mean_yes, lci_yes, uci_yes,
#   d, lower_diff, upper_diff
# )
# my.ci <- round(my.ci, digits = 2)
# return(my.ci)
# }

# get marginally adjmean and CI from rlm
# mmeans_ci <- function(GGEMMEANS)
# {mean <- 100*GGEMMEANS$predicted
# lci <- 100*GGEMMEANS$conf.low
# uci <- 100*GGEMMEANS$conf.high
# d <- 100*(GGEMMEANS$predicted[2] - GGEMMEANS$predicted[1])
# se_diff <-  sqrt(GGEMMEANS$std.error[1]^2 + GGEMMEANS$std.error[2]^2)
# lower_diff <- (d -1.96*se_diff)
# upper_diff <- (d +1.96*se_diff)
# my.ci <- data.frame(cbind(mean, lci, uci, d, lower_diff, upper_diff))
# }
#  
mmeans_ci_vertic_bin <- function(GGEMMEANS) {
  # Convert predicted values and CIs to percentages
  mean_vals <- 100 * GGEMMEANS$predicted
  lci_vals <- 100 * GGEMMEANS$conf.low
  uci_vals <- 100 * GGEMMEANS$conf.high
  
  # Reference group is the first row
  ref_pred <- GGEMMEANS$predicted[1]
  ref_se <- GGEMMEANS$std.error[1]
  
  # Initialize result data frame
  results <- data.frame(
    group = seq_len(nrow(GGEMMEANS)),
    mean = mean_vals,
    lci = lci_vals,
    uci = uci_vals,
    d = NA,
    lower_diff = NA,
    upper_diff = NA
  )
  
  # Loop over rows (starting from second row)
  for (i in 2:nrow(GGEMMEANS)) {
    diff <- 100 * (GGEMMEANS$predicted[i] - ref_pred)
    se_diff <- sqrt(GGEMMEANS$std.error[i]^2 + ref_se^2)
    lower <- diff - 1.96 * se_diff
    upper <- diff + 1.96 * se_diff
    
    results$d[i] <- diff
    results$lower_diff[i] <- lower
    results$upper_diff[i] <- upper
  }
  
  results <- round(results, 2)
  return(results)
}

mmeans_ci_vertic_cont <- function(GGEMMEANS) {
  # Convert predicted values and CIs to percentages
  mean_vals <- GGEMMEANS$predicted
  lci_vals <- GGEMMEANS$conf.low
  uci_vals <-  GGEMMEANS$conf.high
  
  # Reference group is the first row
  ref_pred <- GGEMMEANS$predicted[1]
  ref_se <- GGEMMEANS$std.error[1]
  
  # Initialize result data frame
  results <- data.frame(
    group = seq_len(nrow(GGEMMEANS)),
    mean = mean_vals,
    lci = lci_vals,
    uci = uci_vals,
    d = NA,
    lower_diff = NA,
    upper_diff = NA
  )
  
  # Loop over rows (starting from second row)
  for (i in 2:nrow(GGEMMEANS)) {
    diff <-  (GGEMMEANS$predicted[i] - ref_pred)
    se_diff <- sqrt(GGEMMEANS$std.error[i]^2 + ref_se^2)
    lower <- diff - 1.96 * se_diff
    upper <- diff + 1.96 * se_diff
    
    results$d[i] <- diff
    results$lower_diff[i] <- lower
    results$upper_diff[i] <- upper
  }
  
  results <- round(results, 2)
  return(results)
}