library(here)
library(deSolve)
library(dplyr)
library(data.table)
library(ggplot2)
library(readr)

source(here::here("src/4_transmission_impact/seir_functions.R"))
#' Inputs:
#' - start_population size
#' - times: time steps over which to run the model
#' - n_inf: number infected at the start
#' - R0: required R0
#' - delta: 1/latent period
#' - gamma: 1/duration of infectiousness
#' - red_factor: daily population change, where 1 is the
#'   original population size. I.e. 0.8 = 80% of original population
#' - lockdown_start: number of days in initial R0
#' Manual input is required only in the initial conditions section

# initial conditions

#seq(from = 1, to = 0.5, length.out = 100,)

run_model <- function(
  n_inf,
  R0_start,
  R0_lockdown,
  delta,
  gamma,
  red_factor,
  times,
  start_population,
  lockdown_start
){

  # create parameter list
  theta <- list(beta1 = R0_start*gamma,
                beta2 = R0_lockdown*gamma,
                delta = delta,
                gamma = gamma,
                n_init = start_population,
                red_factor = red_factor,
                lockdown_start = lockdown_start)

  # Calculate the initial state
  init_state <- calc_init_state(pop_size = start_population,
                                n_inf = n_inf)
  # Run the model
  model_output <- as.data.table(ode(y = init_state,
                                    times = times,
                                    func = SEIR_model,
                                    parms = theta))

  # Plot the output
  model_output_m <- melt.data.table(model_output, id.vars = "time")

  return(model_output_m)

}

# Scenarios
# 1: no change - drop - recovery (but at different times)
# 2: no change - drop - recovery (but at different magnitudes)
# 3: slow drop (V shape) (but at different times)
# 4: slow drop (V shape) (but at different magnitudes)
# 5: no change - rise - recovery (but at different times)

changes <- seq(0.5, 2, by = 0.25)


# Scenario 1
timing_changes <- seq(20, 120, by = 15)

s1 <- list()
for (change in timing_changes){
  s1[[as.character(change)]] <- c(rep(1, change), seq(0.8, 1, length.out = 80), rep(1, 200 - (change + 80)))
}

plot(s1[[4]], type = 'l')

# Scenario 2
s2 <- lapply(changes, seq, to = 1, length.out = 120)

concat_no_change_front <- function(v){
  return(c(rep(1, 40), v, rep(1, 40)))
}

s2 <- lapply(s2, concat_no_change_front)

plot(s2[[1]], type = 'l')

# Scenario 3
timing_changes <- seq(20, 120, by = 10)

s3 <- list()

decline <- seq(0.8, 1, length.out = 40)
for (change in timing_changes){
  s3[[as.character(change)]] <- c(rep(1, change), rev(decline), decline, rep(1, 200 - (change + 80)))
}

plot(s3[[1]], type = 'l')

# Scenario 4

s4 <- lapply(changes, seq, to = 1, length.out = 40)

concat_no_change_front_back <- function(v){
  return(c(rep(1, 60), rev(v), v, rep(1, 60)))
}

s4 <- lapply(s4, concat_no_change_front_back)

plot(s4[[1]], type = 'l')

# Scenario 5
timing_changes <- seq(20, 120, by = 15)

s5 <- list()
for (change in timing_changes){
  s5[[as.character(change)]] <- c(rep(1, change), seq(1.2, 1, length.out = 80), rep(1, 200 - (change + 80)))
}

plot(s5[[4]], type = 'l')

# Run model with different scenarios
n_inf <- 300
R0_start <- 3.5
R0_lockdown <- 2
delta <- 1/2
gamma <- 1/8
red_factor <- seq(1, 0.1, length.out=200)
times <- c(1:length(red_factor))
lockdown_start <- 1
start_population <- 1000000

times <- seq(1, 200, by = 1)

run_model_scenario <- function(scenario, scenario_name, time_difference = T){

  time <- tibble(times, scenario)

  if (time_difference){
    type <- max(c(which(scenario == 0.8)[1] - 1, which(scenario == 1.2)[1] - 1), na.rm = T)
  } else {
    type <- max(abs(c(min(scenario), max(scenario))[c(min(scenario), max(scenario)) != 1]))
    type <- ifelse(type == -Inf, 1, type)
  }

  res <- run_model(
    n_inf,
    R0_start,
    R0_lockdown,
    delta,
    gamma,
    scenario,
    times,
    start_population,
    lockdown_start
  ) %>%
    mutate(scenario_name = scenario_name) %>%
    left_join(time, by = c("time" = "times")) %>%
    filter(variable == "i_state") %>%
    mutate(population = scenario,
           type = type)

  return(res)

}

plot_population <- function(res, title){

  p <- res %>%
    mutate(type = factor(type, levels = sort(unique(type)))) %>%
    ggplot() +
    geom_path(aes(x = time, y = population * 100, color = type), size = 0.4) +
    geom_path(data = no_change_res, aes(x = time, y = population * 100), color = "black", size = 0.2) +
    theme_classic() +
    labs(title = title, y = "Population (%)", x = "Time") +
    theme(legend.position = "none")  +
    ylim(c(0, 200))

  return(p)

}

plot_incidence <- function(res, title){

  p <- res %>%
    mutate(type = factor(type, levels = sort(unique(type)))) %>%
    ggplot() +
    geom_path(aes(x = time, y = value * gamma, color = type), size = 0.4) +
    geom_path(data = no_change_res, aes(x = time, y = value * gamma), color = "black", size = 0.2) +
    theme_classic() +
    labs(title = title, y = "Incidence", x = "Time") +
    theme(legend.position = "none")

  return(p)

}

plot_incidence_cumulative <- function(res, title, x_label, axis_text_x_size = 6){

  no_change_point <- no_change_res %>%
    group_by(type) %>%
    summarise(final_size = sum(value * gamma, na.rm = T) / start_population)

  p <- res %>%
    group_by(type) %>%
    summarise(final_size = sum(value * gamma, na.rm = T) / start_population) %>%
    mutate(type = factor(type, levels = sort(unique(type)))) %>%
    ggplot() +
    geom_hline(data = no_change_point, aes(yintercept = final_size),
               color = "black", size = 0.4, linetype="solid") +
    geom_point(aes(x = type, y = final_size, color = type)) +
    theme_classic() +
    labs(title = title, y = "Cumulative Incidence", x = x_label) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = axis_text_x_size)) #+
    #scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 0.01))

  return(p)

}

plot_scenario_res <- function(s_res, title_1, title_2, title_3, x_label_3){

  p_pop <- plot_population(s_res, title_1)
  p_inc <- plot_incidence(s_res, title_2)

  p_inc_cum <- plot_incidence_cumulative(s_res, title=title_3, x_label=x_label_3)

  p <- cowplot::plot_grid(p_pop, p_inc, p_inc_cum, nrow = 1)

  return(p)

}

run_scenario <- function(sc, time_difference,
                         title_1, title_2, title_3, x_label_3){

  sc_res <- lapply(sc, run_model_scenario, scenario_name = "test", time_difference = time_difference)

  print(sc_res)
  sc_res <- do.call(rbind, sc_res)

  p_sc <- plot_scenario_res(sc_res, title_1 = title_1, title_2 = title_2, title_3 = title_3, x_label_3 = x_label_3)
  return(p_sc)

}

no_change_res <- run_model_scenario(rep(1, 200), "test", F) %>%
  mutate(type = "0")

p_s1 <- run_scenario(s1, T, "a", "b", "c", "Population Decline Timing")
p_s2 <- run_scenario(s2, F, "d", "e", "f", "Population Decline Magnitude")
p_s3 <- run_scenario(s3, T, "a", "b", "c", "Population Decline Timing")
p_s4 <- run_scenario(s4, F, "d", "e", "f", "Population Decline Magnitude")
p_s5 <- run_scenario(s5, T, "g", "h", "i", "Population Decline Timing")

p <- cowplot::plot_grid(p_s1, p_s2, ncol = 1)

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/seir_model_sharp_change.png"),
                        11, 6)

p <- cowplot::plot_grid(p_s3, p_s4, p_s5, ncol = 1)

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/seir_supp.png"),
                        11, 8)
