# ==============================================================================
# Persuasive Arguments Theory (PAT) Simulation Module - SIMULATION RUNS
# ==============================================================================

source("functions.R")

# 1. Simulation: Original Study Baseline
# ------------------------------------------------------------------------------
set.seed(111)
simulation_results <- simulate_experiment(
  n_groups = 10, 
  n_agents_per_group = 4,
  pool_size = 1000,
  pool_bias = 0.4,
  n_IA = 4,
  n_AA = 12,
  C = 0
)

# 2. Simulation: Higher N (Robustness Check)
# ------------------------------------------------------------------------------
set.seed(111)
simulation_higherN <- simulate_experiment(
  n_groups = 50, 
  n_agents_per_group = 10,
  pool_size = 1000,
  pool_bias = 0.4,
  n_IA = 4,
  n_AA = 12,
  C = 0
)

# 3. Simulation: With Uncertainty Factor (C)
# ------------------------------------------------------------------------------
set.seed(111)
simulation_C <- simulate_experiment(
  n_groups = 50, 
  n_agents_per_group = 10,
  pool_size = 1000,
  pool_bias = 0.4,
  n_IA = 4,
  n_AA = 12,
  C = 4
)

# SAVE RESULTS
# This allows analysis.R to run without re-simulating everything
save(simulation_results, simulation_higherN, simulation_C, file = "simulation_data.RData")