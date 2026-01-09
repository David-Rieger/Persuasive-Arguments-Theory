# ==============================================================================
# Persuasive Arguments Theory (PAT) Simulation Module
# ==============================================================================

#' Create Group Argument Pool
#'
#' Generates the specific pool of arguments available to the group members.
#'
#' @param pool_size Integer. The total number of arguments in the group's pool.
#' @param pool_bias Numeric. The target mean of the pool (-1 to 1).
#'
#' @return A numeric vector containing the argument values (e.g., -0.3, 0.8).

create_pool <- function(pool_size, pool_bias) {
  # Map Bias [-1, 1] to Beta mean [0, 1]
  mu <- (pool_bias + 1) / 2
  
  # Precision parameter (nu). nu=10 creates a narrow Normal-like shape (Bell curve).
  nu <- 10 
  shape1 <- mu * nu
  shape2 <- (1 - mu) * nu
  
  # Generate [0, 1] and scale to [-1, 1]
  val_01 <- rbeta(pool_size, shape1, shape2)
  arguments <- val_01 * 2 - 1
  
  return(arguments)
}

#' Calculate Psychological Tendency (Damped Average)
#'
#' Calculates the opinion tendency as the mean of held arguments, optionally damped by an
#' uncertainty factor C (**additional assumption**) to model hesitation when information is scarce.
#'
#' @param arguments Numeric vector. The arguments currently held by the agent.
#' @param C Numeric. The uncertainty/damping factor. Must be >= 0.
#'        \itemize{
#'          \item \code{C = 0} (Default): The assumption is inactive. Calculates the standard arithmetic mean.
#'          \item \code{C > 0}: The assumption is active. The value is dampened towards 0 when N is small.
#'        }
#'
#' @return Numeric value between -1 and 1 representing the opinion tendency.

calc_tendency <- function(arguments, C = 0) {
  n <- length(arguments)
  
  # Validate C
  if (C < 0) stop("C must be non-negative (>= 0).")
  
  # Safety check for empty set with no damping
  if (n == 0 && C == 0) return(0)
  
  # Unified Formula: Sum of values divided by (Number of arguments + C)
  numerator <- sum(arguments)
  denominator <- n + C
  
  return(numerator / denominator)
}

#' Initialize Single Agent
#'
#' Creates an agent object, draws the initial set of arguments (IA) from the
#' group pool (without replacement), and calculates the pre-discussion tendency.
#'
#' @param agent_id Integer or String. Unique identifier for the agent.
#' @param pool Numeric vector. The group argument pool.
#' @param n_IA Integer. Number of Initial Arguments the agent holds before discussion.
#' @param C Numeric. The uncertainty factor for the tendency calculation.
#'
#' @return A list containing the agent's ID, current arguments, and T_pre.

initialize_agent <- function(agent_id, pool, n_IA, C) {
  # Draw random arguments from the pool (Initial Arguments)
  ia_i <- sample(pool, size = n_IA, replace = FALSE)
  
  # Calculate Initial Tendency (Pre)
  t_pre <- calc_tendency(ia_i, C)
  
  list(
    id = agent_id,
    arguments = ia_i,
    t_pre = t_pre,
    t_post = NA_real_,  # Placeholder
    shift = NA_real_    # Placeholder
  )
}

#' Discuss and Update Agent
#'
#' Simulates the discussion phase where an agent is exposed to additional arguments (AA) (with replacement).
#' Updates the agent's argument set and calculates the post-discussion tendency.
#'
#' @param agent List. The agent object created by initialize_agent.
#' @param pool Numeric vector. The group argument pool (source of new arguments).
#' @param n_AA Integer. Number of Additional Arguments encountered during discussion.
#' @param C Numeric. The uncertainty factor for the tendency calculation.
#'
#' @return The updated agent list including T_post and Choice Shift.

discuss_and_update <- function(agent, pool, n_AA, C) {
  # Draw additional arguments (AA_i)
  # replace=TRUE simulates that arguments are repeated/reinforced in discussion
  aa_i <- sample(pool, size = n_AA, replace = TRUE)
  
  # Integrate new arguments into memory
  new_arguments <- c(agent$arguments, aa_i)
  
  # Calculate Resulting Tendency (Post)
  t_post <- calc_tendency(new_arguments, C)
  
  # Update Agent Object
  agent$arguments <- new_arguments
  agent$t_post <- t_post
  agent$shift <- t_post - agent$t_pre
  
  return(agent)
}

#' Calculate Group Polarization
#'
#' Aggregates the shifts of all agents to determine the group-level polarization.
#'
#' @param agents_list List of agent objects (after discussion).
#'
#' @return Numeric value. Positive indicates shift towards positive extreme,
#'         negative indicates shift towards negative extreme.

calculate_group_polarization <- function(agents_list) {
  all_pre <- sapply(agents_list, function(x) x$t_pre)
  all_post <- sapply(agents_list, function(x) x$t_post)
  
  mean_pre <- mean(all_pre)
  mean_post <- mean(all_post)
  
  return(mean_post - mean_pre)
}

#' Run Complete PAT Simulation
#'
#' Orchestrates the full simulation process: creating the group pool, initializing agents
#' with Initial Arguments (IA), running the discussion with Additional Arguments (AA),
#' and calculating results.
#'
#' @param n_agents Integer. Number of agents in the group.
#' @param pool_size Integer. Size of the group argument pool.
#' @param pool_bias Numeric (-1 to 1). Bias of the group's pool.
#'        0 = Neutral, 0.8 = Strong Pro, -0.5 = Moderate Contra.
#' @param n_IA Integer. Number of Initial Arguments per agent.
#' @param n_AA Integer. Number of Additional Arguments added during discussion.
#' @param C Numeric. Uncertainty/Damping factor. Default is 0.
#'
#' @return A list containing configuration, individual agent data frame, and global GP metric.

run_simulation <- function(n_agents = 10,
                           pool_size = 1000,
                           pool_bias = 0.6,
                           n_IA = 3,
                           n_AA = 20,
                           C = 0) {
  
  # 1. Create the environment (Group Context)
  group_pool <- create_pool(pool_size, pool_bias)
  
  # 2. Initialize Agents (Individual Level - Pre)
  agents <- vector("list", n_agents)
  for(i in 1:n_agents) {
    agents[[i]] <- initialize_agent(i, group_pool, n_IA, C)
  }
  
  # 3. Discussion Process (Individual Level - Update)
  for(i in 1:n_agents) {
    agents[[i]] <- discuss_and_update(agents[[i]], group_pool, n_AA, C)
  }
  
  # 4. Aggregation (Group Level)
  gp <- calculate_group_polarization(agents)
  
  # 5. Format Results
  results_df <- data.frame(
    AgentID = sapply(agents, function(x) x$id),
    T_pre = round(sapply(agents, function(x) x$t_pre), 3),
    T_post = round(sapply(agents, function(x) x$t_post), 3),
    Shift = round(sapply(agents, function(x) x$shift), 3)
  )
  
  list(
    Config = list(
      N_Agents = n_agents,
      Pool_Bias = pool_bias,
      C_Factor = C,
      N_Initial_Args = n_IA,
      N_Additional_Args = n_AA
    ),
    Individual_Data = results_df,
    Group_Polarization = gp
  )
}
