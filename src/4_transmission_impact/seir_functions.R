# Functions script

# the SEIR model
SEIR_model <- function(time, state, theta) {
  
  # reduction factor (based on time)
  red_all <- theta[["red_factor"]]
  red <- red_all[ceiling(time)]
  
  ## define variables
  S <- state[["s_state"]]
  E <- state[["e_state"]]
  I <- state[["i_state"]]
  R <- state[["r_state"]]
 
  # current population, population last step, and initial population
  N <- S+E+I+R
  N_prev <- state[["N_state"]]
  N_init <- theta[["n_init"]]
  
  # extract parameters
  lockdown_start <- theta[["lockdown_start"]]
  delta <- theta[["delta"]]
  gamma <- theta[["gamma"]]
  if(time < lockdown_start){ beta <- theta[["beta1"]]
  } else if(lockdown_start){
    beta <- theta[["beta2"]]
  }
  # Force of Infection
  foi <- beta*(I/N)
  
  # Define transition equations
  dS <- -S*foi - (S/N)*(N_prev - N_init*red)
  dE <- S*foi- delta*E - (E/N)*(N_prev - N_init*red)
  dI <- delta*E-gamma*I - (I/N)*(N_prev - N_init*red)
  dR <- gamma*I - (R/N)*(N_prev - N_init*red)
  dN <- - (N_prev - N_init*red) # tracking the population size
 
  return(list(c(dS,dE,dI,dR, dN)))
}

# calculate the inital state, depending on population size
calc_init_state <- function(pop_size, n_inf =1){
  
  init_state <- c(
    s_state = pop_size-n_inf, 
    e_state = 0,
    i_state = n_inf,
    r_state = 0, 
    N_state = pop_size)
  
  return(init_state)
  
}
