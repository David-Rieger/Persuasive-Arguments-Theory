# ------------------------------------------------------------------------------
# Statistical Analysis (Hypothesis Testing)
# ------------------------------------------------------------------------------

source("01_functions.R")
load("simulation_data.RData")

# Test 1: Original Study Baseline (C=0)
print("--- Result 1: Original Study ---")
print(t.test(simulation_results$T_post, simulation_results$T_pre, paired = TRUE))

# Test 2: Higher Sample Size (Robustness)
print("--- Result 2: Higher N (Robustness) ---")
print(t.test(simulation_higherN$T_post, simulation_higherN$T_pre, paired = TRUE))

# Test 3: Model with Uncertainty Factor (C=4)
print("--- Result 3: With Uncertainty (C=4) ---")
print(t.test(simulation_C$T_post, simulation_C$T_pre, paired = TRUE))