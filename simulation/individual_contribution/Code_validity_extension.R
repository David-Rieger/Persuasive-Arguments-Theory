# ==============================================================================
# Persuasive Arguments Theory (PAT) Simulation Module
# Paradigm: Weighted Average with Dynamic Confirmation Bias (Validity Filtering)
# ==============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)

#' Create Binary Argument Pool
#'
#' Generates the specific pool of arguments available to the group members.
#' This function creates a strict binary pool consisting only of -1 (Contra)
#' and +1 (Pro) arguments.
#'
#' @param pool_size Integer. The total number of arguments to generate.
#' @param pool_bias Numeric. The target directional tendency (-1 to 1).
#'        Maps to probability: 0.0 is neutral, 0.8 is strong Pro, -1.0 is pure Contra.
#'
#' @return A numeric vector containing only the values -1 and 1.

create_pool <- function(pool_size, pool_bias) {
  # Map Bias [-1, 1] to Probability [0, 1]
  prob_pos <- (pool_bias + 1) / 2
  
  # Sample strict binary arguments
  arguments <- sample(c(-1, 1), size = pool_size, replace = TRUE, 
                      prob = c(1 - prob_pos, prob_pos))
  
  return(arguments)
}

#' Initialize Single Agent
#'
#' Creates an agent object and establishes their "Status Quo" opinion.
#' The agent draws Initial Arguments (IA) from the pool. At this stage, all
#' arguments are treated as fully valid (weight = 1.0) because they form the
#' agent's baseline identity.
#'
#' @param agent_id Integer or String. Unique identifier for the agent.
#' @param pool Numeric vector. The group argument pool (binary values).
#' @param n_IA Integer. Number of Initial Arguments held before discussion.
#'
#' @return A list containing the agent ID, weighted sums, and T_pre.

initialize_agent <- function(agent_id, pool, n_IA) {
  # 1. Draw Initial Arguments (IA) without replacement
  ia_values <- sample(pool, size = n_IA, replace = FALSE)
  
  # 2. Assign weights (Identity Phase: All arguments are fully valid)
  ia_weights <- rep(1, n_IA) 
  
  # 3. Calculate Initial Tendency (Weighted Average)
  sum_val <- sum(ia_values * ia_weights)
  sum_w   <- sum(ia_weights)
  
  # Safety check for division by zero
  t_pre <- if(sum_w == 0) 0 else sum_val / sum_w
  
  list(
    id = agent_id,
    sum_val = sum_val, 
    sum_w   = sum_w,   
    t_pre   = t_pre,
    t_post  = NA_real_,
    shift   = NA_real_
  )
}

#' Discuss and Update Agent (Sequential Validity Processing)
#'
#' Simulates the discussion phase where the agent is exposed to Additional Arguments (AA).
#' Implements a Dynamic Confirmation Bias: Consonant arguments (matching current T) are
#' perceived as fully valid (Weight = 1.0). Dissonant arguments are discounted based on
#' the strength of the current opinion.
#'
#' Formula for Dissonant Weight: w = 1 - (|T| * conf_bias_strength)
#'
#' @param agent List. The agent object created by initialize_agent.
#' @param pool Numeric vector. The group argument pool.
#' @param n_AA Integer. Number of Additional Arguments to encounter.
#' @param conf_bias_strength Numeric. Factor (0 to 1) controlling bias intensity.
#'        0.0 is rational processing; 1.0 is maximum bias (strong opinions ignore dissent).
#'
#' @return The updated agent list including T_post and shift.

discuss_and_update <- function(agent, pool, n_AA, conf_bias_strength = 1) {
  
  # Load current state
  current_sum_val <- agent$sum_val
  current_sum_w   <- agent$sum_w
  current_t       <- agent$t_pre
  
  # Process new arguments sequentially
  for(k in 1:n_AA) {
    # 1. Draw ONE new argument (with replacement)
    new_arg <- sample(pool, size = 1, replace = TRUE)
    
    # 2. Check Alignment
    is_consonant <- (new_arg * current_t) >= 0 
    
    # 3. Calculate Validity Weight
    if (is_consonant) {
      weight <- 1.0
    } else {
      # Discount dissonant arguments based on opinion strength
      weight <- 1.0 - (abs(current_t) * conf_bias_strength)
      if(weight < 0) weight <- 0
    }
    
    # 4. Update Weighted Average
    current_sum_val <- current_sum_val + (new_arg * weight)
    current_sum_w   <- current_sum_w + weight
    
    # Immediate feedback: Update T for the next argument
    current_t <- current_sum_val / current_sum_w
  }
  
  # Save final results
  agent$t_post  <- current_t
  agent$shift   <- agent$t_post - agent$t_pre
  agent$sum_val <- current_sum_val
  agent$sum_w   <- current_sum_w
  
  return(agent)
}

#' Run Complete PAT Simulation
#'
#' Orchestrates the full simulation process: creating the binary pool, initializing agents,
#' running the sequential discussion with validity filtering, and calculating results.
#'
#' @param n_agents Integer. Number of agents in the group.
#' @param pool_size Integer. Total number of binary arguments in the pool.
#' @param pool_bias Numeric. Target directional tendency (-1 to 1).
#' @param n_IA Integer. Number of Initial Arguments per agent.
#' @param n_AA Integer. Number of Additional Arguments per agent.
#' @param conf_bias_strength Numeric. Strength of confirmation bias (0 to 1). Default 1.0.
#'
#' @return A list containing Config, Individual_Data, and Group_Polarization.

run_simulation <- function(n_agents = 10, 
                           pool_size = 1000, 
                           pool_bias = 0.5, 
                           n_IA = 3, 
                           n_AA = 20, 
                           conf_bias_strength = 1.0) {
  
  # 1. Create Environment
  group_pool <- create_pool(pool_size, pool_bias)
  
  # 2. Initialize Agents
  agents <- vector("list", n_agents)
  for(i in 1:n_agents) {
    agents[[i]] <- initialize_agent(i, group_pool, n_IA)
  }
  
  # 3. Discussion Process
  for(i in 1:n_agents) {
    agents[[i]] <- discuss_and_update(agents[[i]], group_pool, n_AA, conf_bias_strength)
  }
  
  # 4. Aggregation
  all_pre <- sapply(agents, function(x) x$t_pre)
  all_post <- sapply(agents, function(x) x$t_post)
  
  gp <- mean(all_post) - mean(all_pre)
  
  results_df <- data.frame(
    AgentID = sapply(agents, function(x) x$id),
    T_pre = round(all_pre, 3),
    T_post = round(all_post, 3),
    Shift = round(all_post - all_pre, 3)
  )
  
  list(
    Config = list(
      n_agents = n_agents, 
      pool_bias = pool_bias, 
      conf_bias = conf_bias_strength,
      n_IA = n_IA,
      n_AA = n_AA
    ),
    Individual_Data = results_df,
    Group_Polarization = gp
  )
}

#' Simulate Full Experiment (Multi-Group)
#'
#' Simulates a complete experiment comprising multiple independent groups,
#' mirroring the design of studies like Moscovici & Zavalloni (1969).
#' It aggregates individual-level data from all groups into a single dataset
#' suitable for statistical hypothesis testing (e.g., t-test).
#'
#' @param n_groups Integer. Number of independent groups to simulate (e.g., 10).
#' @param n_agents_per_group Integer. Number of agents per group (e.g., 4).
#' @param ... Additional arguments passed to run_simulation (e.g., pool_bias, conf_bias_strength).
#'
#' @return A data frame containing data for all agents across all groups,
#'         including a 'GroupID' column.
simulate_experiment <- function(n_groups = 10, n_agents_per_group = 4, ...) {
  
  # List to store result dataframes from each group
  all_data_list <- list()
  
  # Argument list for do.call
  args_list <- list(...)
  # Add the specific n_agents for this wrapper
  args_list$n_agents <- n_agents_per_group
  
  for (g in 1:n_groups) {
    # Run simulation for one group
    # do.call is used to pass the variable arguments (...) cleanly
    sim_result <- do.call(run_simulation, args_list)
    
    # Extract individual data
    df <- sim_result$Individual_Data
    
    # Add GroupID to distinguish this group's members
    df$GroupID <- g
    
    # Store in list
    all_data_list[[g]] <- df
  }
  
  # Merge all group dataframes into one large dataset
  full_dataset <- do.call(rbind, all_data_list)
  
  return(full_dataset)
}

# ==============================================================================
# EXECUTION & EVALUATION
# ==============================================================================

# 1. Run Subjective Validity Simulation
set.seed(1)
simulation_validity <- simulate_experiment(
  n_groups = 50, 
  n_agents_per_group = 10,
  pool_size = 1000,
  pool_bias = 0.4,
  n_IA = 4,
  n_AA = 12,
  conf_bias_strength = 1.0
)

# 2. Paired t-Test
print("--- Result: Subjective Validity Model ---")
validity_ttest <- t.test(simulation_validity$T_post, simulation_validity$T_pre, paired = TRUE)
print(validity_ttest)

# 3. Data Preparation for Plotting
plot_data_validity <- simulation_validity %>%
  mutate(UniqueID = paste0("G", GroupID, "_A", AgentID)) %>%
  select(UniqueID, T_pre, T_post) %>%
  pivot_longer(
    cols = c("T_pre", "T_post"), 
    names_to = "Phase", 
    values_to = "Value"
  ) %>%
  mutate(Phase = factor(Phase, levels = c("T_pre", "T_post")))

# Sample random individuals for trajectory lines
set.seed(123)
sampled_ids_val <- sample(unique(plot_data_validity$UniqueID), 10)
sampled_data_val <- plot_data_validity %>% filter(UniqueID %in% sampled_ids_val)

# 4. Boxplot Output
p_validity <- ggplot(plot_data_validity, aes(x = Phase, y = Value)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", size = 11),
    axis.text = element_text(color = "black")
  ) +
  
  # Trajectories (Background)
  geom_line(data = sampled_data_val, aes(group = UniqueID), color = "grey70", alpha = 0.5, linewidth = 0.6) +
  geom_point(data = sampled_data_val, size = 2, color = "grey70", alpha = 0.6) +
  
  # Boxplot
  geom_boxplot(fill = "white", color = "black", width = 0.4, outlier.shape = 1, linewidth = 0.6) +
  
  # Means
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "firebrick", color = "black", stroke = 1) +
  stat_summary(fun = mean, geom = "text", aes(label = sprintf("%.2f", after_stat(y))), 
               vjust = -2.5, color = "firebrick", fontface = "bold", size = 3.5) +
  
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  scale_x_discrete(labels = c("Pre-Discussion", "Post-Discussion")) +
  
  labs(
    title = "Group Polarization Effect",
    subtitle = "Subjective Validity Model (Dynamic Confirmation Bias)",
    y = "Opinion Tendency",
    x = NULL
  )

ggsave("individual_contribution/validity_group_polarization_result.png", plot = p_validity, width = 8, height = 6, dpi = 300)