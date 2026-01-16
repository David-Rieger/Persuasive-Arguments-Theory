# ==============================================================================
# Create Binary Argument Pool
# ==============================================================================

create_pool <- function(pool_size, pool_bias = 0) {
  # pool_bias in [-1, 1] bestimmt Wahrscheinlichkeit für Pro-Argumente
  p_pro <- (pool_bias + 1) / 2
  
  arguments <- sample(
    c(-1, 1),
    size = pool_size,
    replace = TRUE,
    prob = c(1 - p_pro, p_pro)
  )
  
  return(arguments)
}

# ==============================================================================
# Compute Tendency from weighted arguments
# ==============================================================================

compute_tendency <- function(arguments, weights) {
  t <- sum(arguments * weights) / sum(weights)
  return(max(-1, min(1, t)))
}

# ==============================================================================
# Initialize Agent
# ==============================================================================

initialize_agent <- function(agent_id, pool, n_IA) {
  
  ia <- sample(pool, n_IA, replace = FALSE)
  weights <- rep(1, n_IA)
  
  t_init <- compute_tendency(ia, weights)
  
  list(
    id = agent_id,
    arguments = ia,
    weights = weights,
    tendency_history = t_init
  )
}

# ==============================================================================
# Validity Function
# ==============================================================================

compute_validity <- function(current_tendency, alpha = 1) {
  return(1 + alpha * abs(current_tendency))
}

# ==============================================================================
# Process a Single New Argument
# ==============================================================================

process_argument <- function(agent, new_argument, alpha = 1) {
  
  current_t <- tail(agent$tendency_history, 1)
  validity <- compute_validity(current_t, alpha)
  
  # Kongruenz prüfen
  if (sign(new_argument) == sign(current_t) && current_t != 0) {
    weight <- validity
  } else {
    weight <- 1
  }
  
  # Update memory
  agent$arguments <- c(agent$arguments, new_argument)
  agent$weights <- c(agent$weights, weight)
  
  # Recalculate tendency
  new_t <- compute_tendency(agent$arguments, agent$weights)
  agent$tendency_history <- c(agent$tendency_history, new_t)
  
  return(agent)
}

# ==============================================================================
# Discussion Phase
# ==============================================================================

discuss_and_update <- function(agent, pool, n_AA, alpha = 1) {
  
  aa <- sample(pool, n_AA, replace = TRUE)
  
  for (arg in aa) {
    agent <- process_argument(agent, arg, alpha)
  }
  
  return(agent)
}

# ==============================================================================
# Group Polarization
# ==============================================================================

calculate_group_polarization <- function(agents) {
  
  pre <- sapply(agents, function(a) a$tendency_history[1])
  post <- sapply(agents, function(a) tail(a$tendency_history, 1))
  
  mean(post) - mean(pre)
}

# ==============================================================================
# Run Simulation
# ==============================================================================

run_simulation <- function(n_agents = 10,
                           pool_size = 100,
                           pool_bias = 0,
                           n_IA = 3,
                           n_AA = 10,
                           alpha = 1) {
  
  pool <- create_pool(pool_size, pool_bias)
  
  agents <- lapply(1:n_agents, function(i) {
    initialize_agent(i, pool, n_IA)
  })
  
  agents <- lapply(agents, discuss_and_update,
                   pool = pool,
                   n_AA = n_AA,
                   alpha = alpha)
  
  gp <- calculate_group_polarization(agents)
  
  results <- data.frame(
    AgentID = sapply(agents, `[[`, "id"),
    T_pre = round(sapply(agents, function(a) a$tendency_history[1]), 3),
    T_post = round(sapply(agents, function(a) tail(a$tendency_history, 1)), 3)
  )
  
  list(
    Individual_Data = results,
    Group_Polarization = gp,
    Agents = agents
  )
}

# ==============================================================================
# Example
# ==============================================================================

run_simulation(
  n_agents = 5,
  pool_size = 30,
  pool_bias = 0.4,
  n_IA = 4,
  n_AA = 6,
  alpha = 1.5
)
