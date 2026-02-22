# ==============================================================================
# Persuasive Arguments Theory (PAT) Simulation Module - ANALYSIS & PLOTS
# ==============================================================================

source("functions.R")      # Load plotting themes/libs implicitly
load("simulation_data.RData") # Load the results from simulation.R
library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# PART 1: Function Explainer Plots (Demos)
# ------------------------------------------------------------------------------

# --- Demo 1: Pool Distribution (create_pool) ---

# 1. Simulate Data (Demo)
set.seed(42)
pool_data <- create_pool(pool_size = 10000, pool_bias = 0.4)

# 2. Visualization
p1 <- ggplot(data.frame(Value = pool_data), aes(x = Value)) +
  theme_bw(base_size = 14) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0.4, color = "firebrick", size = 1.2, linetype = "dashed") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  labs(
    title = "Distribution of Argument Valence in the Generated Pool",
    subtitle = expression(paste("Simulated using a scaled Beta-distribution with bias parameter ", mu == 0.4)),
    x = expression(paste("Argument Valence (", italic(v), ") [-1: Contra, +1: Pro]")),
    y = "Frequency"
  )

ggsave("plots/01_pool_distribution.png", plot = p1, width = 8, height = 5, dpi = 300)

# --- Demo 2: Dampening Effect (calc_tendency) ---

# 1. Simulate Data (Demo)
set.seed(42)
pool_vector <- create_pool(pool_size = 1000, pool_bias = 0.5)
n_agents <- 30
sim_data <- data.frame(id = 1:n_agents, Pure_Mean = NA, Damped_Tendency = NA)

for (i in 1:n_agents) {
  agent <- initialize_agent(i, pool_vector, n_IA = 3, C = 4)
  sim_data$Damped_Tendency[i] <- agent$t_pre
  sim_data$Pure_Mean[i] <- mean(agent$arguments)
}

# 2. Visualization
p2 <- ggplot(sim_data) +
  theme_classic(base_size = 12) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = Pure_Mean, xend = Pure_Mean, y = Pure_Mean, yend = Damped_Tendency),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey70") +
  geom_point(aes(x = Pure_Mean, y = Damped_Tendency), size = 3, color = "steelblue") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "Impact of Uncertainty on Initial Opinion Formation",
    subtitle = expression(paste("Comparison of arithmetic mean vs. damped tendency with uncertainty factor ", italic(C) == 4)),
    x = "Arithmetic Mean of Arguments (Raw Opinion)",
    y = expression(paste("Calculated Tendency (", italic(T)[pre], ")"))
  )

ggsave("plots/02_dampening_effect.png", plot = p2, width = 7, height = 6, dpi = 300)

# --- Demo 3: Theoretical System Behavior ---

# 1. Simulate Data (Demo)
set.seed(101)
pool_vector_sys <- create_pool(pool_size = 5000, pool_bias = 0.6)

# Initialize agent with 0 arguments to visualize the complete mathematical curve
agent_sys <- initialize_agent(agent_id = 1, pool = pool_vector_sys, n_IA = 0, C = 5) 

steps <- 30
history <- data.frame(Step = 0:steps, N_Arguments = NA, Tendency = NA)
history$N_Arguments[1] <- 0
history$Tendency[1] <- 0 

for (i in 1:steps) {
  agent_sys <- discuss_and_update(agent_sys, pool_vector_sys, n_AA = 1, C = 5)
  history$N_Arguments[i + 1] <- length(agent_sys$arguments)
  history$Tendency[i + 1] <- agent_sys$t_post
}

# 2. Visualization
p3 <- ggplot(history, aes(x = N_Arguments, y = Tendency)) +
  theme_classic(base_size = 14) +
  geom_hline(yintercept = 0.6, color = "darkgreen", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 8, y = 0.65, label = "Structural Bias Asymptote (0.6)", color = "darkgreen", fontface = "bold") +
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(size = 3, color = "black") +
  scale_y_continuous(limits = c(-0.1, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Theoretical Behavior of the Complete System",
    subtitle = expression(paste("Transformation of tendency as new arguments are integrated (", italic(C) == 5, ")")),
    x = expression(paste("Total Number of Arguments Held (", n[i]^{IA} + n[i]^{AA}, ")")),
    y = expression(Resulting~Tendency~"("~italic(T)[i]~")")
  )

ggsave("plots/03_system_behavior.png", plot = p3, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# PART 2: Main Experiment Visualization (Group Polarization)
# ------------------------------------------------------------------------------

# 1. Data Preparation (using Simulation C Results)
plot_data <- simulation_C %>%
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
sampled_ids <- sample(unique(plot_data$UniqueID), 10)
sampled_data <- plot_data %>% filter(UniqueID %in% sampled_ids)

# 2. Plotting
p4 <- ggplot(plot_data, aes(x = Phase, y = Value)) +
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
  geom_line(data = sampled_data, aes(group = UniqueID), color = "grey70", alpha = 0.5, size = 0.6) +
  geom_point(data = sampled_data, size = 2, color = "grey70", alpha = 0.6) +
  
  # Boxplot
  geom_boxplot(fill = "white", color = "black", width = 0.4, outlier.shape = 1, size = 0.6) +
  
  # Means
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "firebrick", color = "black", stroke = 1) +
  stat_summary(fun = mean, geom = "text", aes(label = sprintf("%.2f", after_stat(y))), 
               vjust = -2.5, color = "firebrick", fontface = "bold", size = 3.5) +
  
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  scale_x_discrete(labels = c("Pre-Discussion", "Post-Discussion")) +
  
  labs(
    title = "Group Polarization Effect",
    subtitle = "Mean shifts (red) and individual trajectories",
    y = "Opinion Tendency",
    x = NULL
  )

ggsave("plots/04_group_polarization_result.png", plot = p4, width = 8, height = 6, dpi = 300)
