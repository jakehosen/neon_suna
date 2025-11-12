#produced with https://claude.ai/share/34ad2d84-5179-4ae3-8fef-7b0c92380912

library(deSolve)
library(tidyverse)

# Example: Create or load your data
set.seed(123)
time_hours <- seq(0, 24, by = 0.25)  # 15-minute intervals (0.25 hours)
PAR <- 1000 * pmax(0, sin(pi * (time_hours - 6) / 12))  # Simulate daytime PAR
Q <- 50 + 20 * sin(2 * pi * time_hours / 24) + rnorm(length(time_hours), 0, 5)  # Simulate discharge (m³/s)
Q <- pmax(Q, 10)  # Keep discharge positive

# Define the ODE function with recharge scaled by Q
photodeg_ode <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Interpolate PAR at time t
    PAR_t <- approx(par_times, par_values, xout = t, rule = 2)$y
    
    # Interpolate Q at time t
    Q_t <- approx(q_times, q_values, xout = t, rule = 2)$y
    
    # Differential equation: dUV254/dt = -photodeg * PAR + recharge * Q
    dUV254 <- -photodeg * PAR_t + recharge * (Q_t^2)
    
    list(dUV254)
  })
}

# Function to solve ODE given parameter values
solve_photodeg <- function(params, UV254_initial, times, PAR_data, Q_data, par_times, q_times) {
  parameters <- list(
    photodeg = params[1], 
    recharge = params[2],
    par_values = PAR_data,
    par_times = par_times,
    q_values = Q_data,
    q_times = q_times
  )
  
  state <- c(UV254 = UV254_initial)
  
  out <- ode(y = state, times = times, func = photodeg_ode, parms = parameters)
  return(as.data.frame(out))
}

# Fit both photodeg and recharge to observed data
fit_photodeg_recharge <- function(UV254_obs, UV254_initial, times, PAR_data, Q_data) {
  
  # Objective function: minimize sum of squared residuals
  objective <- function(params) {
    photodeg_value <- params[1]
    recharge_value <- params[2]
    
    # Constrain to reasonable values
    if (photodeg_value < 0 || photodeg_value > 1) return(1e10)
    if (recharge_value < -0.01 || recharge_value > 0.01) return(1e10)
    
    predicted <- solve_photodeg(params, UV254_initial, times, PAR_data, Q_data, times, times)
    residuals <- UV254_obs - predicted$UV254
    sum(residuals^2)
  }
  
  # Initial parameter guesses
  initial_params <- c(photodeg = 1e-5, recharge = 1e-4)
  
  # Optimize using Nelder-Mead
  result <- optim(par = initial_params, 
                  fn = objective, 
                  method = "Nelder-Mead",
                  control = list(maxit = 5000))
  
  return(list(
    photodeg = result$par[1],
    recharge = result$par[2],
    convergence = result$convergence,
    value = result$value
  ))
}

# Example usage with simulated data
# ----------------------------------

# Simulate "observed" data
UV254_initial <- 0.15
true_photodeg <- 2e-5
true_recharge <- 1e-6  # Now represents recharge per unit discharge
true_params <- c(true_photodeg, true_recharge)

observed_data <- solve_photodeg(true_params, UV254_initial, time_hours, PAR, Q, time_hours, time_hours)
observed_data$UV254_obs <- observed_data$UV254 + rnorm(nrow(observed_data), 0, 0.002)

# Fit the model
fit_result <- fit_photodeg_recharge(
  UV254_obs = observed_data$UV254_obs,
  UV254_initial = UV254_initial,
  times = time_hours,
  PAR_data = PAR,
  Q_data = Q
)

cat("Estimated parameters:\n")
cat("  photodeg:", fit_result$photodeg, "\n")
cat("  recharge:", fit_result$recharge, "\n")
cat("  Convergence:", fit_result$convergence, "(0 = success)\n\n")

cat("True parameters:\n")
cat("  photodeg:", true_photodeg, "\n")
cat("  recharge:", true_recharge, "\n")

# Calculate R-squared
predicted <- solve_photodeg(c(fit_result$photodeg, fit_result$recharge), 
                           UV254_initial, time_hours, PAR, Q, time_hours, time_hours)
ss_res <- sum((observed_data$UV254_obs - predicted$UV254)^2)
ss_tot <- sum((observed_data$UV254_obs - mean(observed_data$UV254_obs))^2)
r_squared <- 1 - ss_res/ss_tot
cat("\nR-squared:", round(r_squared, 4), "\n")

# Plot results
plot_data <- data.frame(
  time = time_hours,
  observed = observed_data$UV254_obs,
  predicted = predicted$UV254,
  PAR = PAR,
  Q = Q
)

p1 <- ggplot(plot_data, aes(x = time)) +
  geom_point(aes(y = observed), alpha = 0.5, size = 2) +
  geom_line(aes(y = predicted), color = "blue", size = 1) +
  labs(x = "Time (hours)", y = "UV254", 
       title = "UV254 Photodegradation + Discharge-Scaled Recharge Model") +
  theme_minimal()

p2 <- ggplot(plot_data, aes(x = time, y = PAR)) +
  geom_line(color = "orange", size = 1) +
  labs(x = "Time (hours)", y = "PAR (μmol photons m⁻² s⁻¹)", 
       title = "PAR Time Series") +
  theme_minimal()

p3 <- ggplot(plot_data, aes(x = time, y = Q)) +
  geom_line(color = "steelblue", size = 1) +
  labs(x = "Time (hours)", y = "Discharge (m³/s)", 
       title = "Discharge Time Series") +
  theme_minimal()

library(patchwork)
p1 / p2 / p3

# Residual plot
plot_data$residuals <- plot_data$observed - plot_data$predicted

ggplot(plot_data, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted UV254", y = "Residuals",
       title = "Residual Plot") +
  theme_minimal()

# Optional: Plot the contribution of each process
plot_data$photodeg_term <- -fit_result$photodeg * plot_data$PAR
plot_data$recharge_term <- fit_result$recharge * plot_data$Q

ggplot(plot_data, aes(x = time)) +
  geom_line(aes(y = photodeg_term, color = "Photodegradation"), size = 1) +
  geom_line(aes(y = recharge_term, color = "Recharge"), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Photodegradation" = "red", "Recharge" = "blue")) +
  labs(x = "Time (hours)", y = "dUV254/dt contribution", 
       title = "Process Contributions to UV254 Change",
       color = "Process") +
  theme_minimal()