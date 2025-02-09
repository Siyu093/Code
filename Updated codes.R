library(dplyr)

df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(195, 197),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))


library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(190, 193),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(20, 16),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(181, 120),
  total   = c(195, 197)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(69, 10),
  total   = c(195, 197)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))


library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(111, 108),
  total   = c(195, 197)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))


library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(111, 127),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))


library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(108, 114),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(48, 63),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(207, 207),
  total   = c(240, 230)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(216, 227),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(212, 222),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(22, 19),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(197, 142),
  total   = c(216, 227)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(73, 15),
  total   = c(216, 227)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(121, 124),
  total   = c(216, 227)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(124, 144),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(120, 131),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(54, 70),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))

library(dplyr)


df <- data.frame(
  group   = c("Intervention", "Control"),
  outcome = c(228, 238),
  total   = c(266, 265)
)

# Calculate event rate (risk) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate effect measures:
# Risk Ratio (RR), Absolute Risk Reduction (ARR), and Number Needed to Treat (NNT)
rr  <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]  # ARR = risk difference (Intervention - Control)
nnt <- 1 / arr

# For non-inferiority analysis, we compute the confidence intervals using a z-value of 1.645
# (corresponding to a one-sided alpha of 0.05 or a two-sided 90% CI)

# --- Risk Ratio CI (using log transformation) ---
rr_se <- sqrt((1 / df$outcome[1] - 1 / df$total[1]) + (1 / df$outcome[2] - 1 / df$total[2]))
rr_lower <- exp(log(rr) - 1.645 * rr_se)
rr_upper <- exp(log(rr) + 1.645 * rr_se)

# --- ARR (Risk Difference) CI ---
se_arr <- sqrt((df$risk[1] * (1 - df$risk[1]) / df$total[1]) + (df$risk[2] * (1 - df$risk[2]) / df$total[2]))
arr_lower <- arr - 1.645 * se_arr
arr_upper <- arr + 1.645 * se_arr

# --- NNT CI ---
nnt_lower <- 1 / arr_upper
nnt_upper <- 1 / arr_lower

# --- Difference in Proportions in Percentage Points ---
diff_prop <- arr * 100
diff_prop_lower <- arr_lower * 100
diff_prop_upper <- arr_upper * 100

# --- Non-Inferiority Analysis ---
# Define the non-inferiority margin (for ARR, a negative margin indicates how much worse the intervention can be)
NI_margin <- -0.10  # e.g., -10%

# Determine if non-inferiority is established:
# For NI, the lower bound of the ARR CI must be greater than the NI margin.
noninferiority <- ifelse(arr_lower > NI_margin, TRUE, FALSE)

# -----------------------------
# Calculate one-sided p-value for the difference in proportions between groups
# -----------------------------

# Create a contingency table: first column is event count, second is non-event count
contingency_table <- matrix(c(df$outcome, df$total - df$outcome), nrow = 2, byrow = FALSE)
rownames(contingency_table) <- df$group
colnames(contingency_table) <- c("Event", "No Event")

# Use prop.test with a one-sided alternative.
# Here, we test the alternative hypothesis that the event rate in the Intervention group is less than in the Control group.
test_result_one_sided <- prop.test(x = df$outcome, n = df$total, alternative = "less")
p_value_one_sided <- test_result_one_sided$p.value

# Print the contingency table and one-sided p-value
print("Contingency Table:")
print(contingency_table)
cat(paste0("One-sided p-value (Intervention < Control): ", round(p_value_one_sided, 4), "\n"))

# -----------------------------
# Print all results
# -----------------------------
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), 
           " (90% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", 
           format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), 
           " (90% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", 
           format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), 
           " (90% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", 
           format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Difference in Proportions: ", format(round(diff_prop, 2), nsmall = 2), "%", 
           " (90% CI: ", format(round(diff_prop_lower, 2), nsmall = 2), "%, ", 
           format(round(diff_prop_upper, 2), nsmall = 2), "%)\n"))