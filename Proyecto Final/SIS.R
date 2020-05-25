library(deSolve)
sis_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  I = state_values [2]        # infectious
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I) + (gamma * I)
      dI = ( beta * S * I) - (gamma * I)
      
      # combine results
      results = c (dS, dI)
      list (results)
    }
  )
}

contact_rate = 5                    # number of contacts per day 
transmission_probability = 0.3       # transmission probability
infectious_period = 15 
beta_value =  contact_rate * transmission_probability
gamma_value = 1 / infectious_period
Ro = beta_value / gamma_value
parameter_list = c (beta = beta_value, gamma = gamma_value)
X = 2999999      # susceptible hosts
Y = 0.00001       # infectious hosts

N = X + Y 
initial_values = c (S = X/N, I = Y/N)
timepoints = seq (0, 77, by=1)
output = lsoda (initial_values, timepoints, sis_model, parameter_list)
# susceptible hosts over time
plot (S ~ time, data = output, type='l', ylim = c(0,1), col = 'blue', ylab = 'S, I, S', main = 'SIS epidemic') 

# remain on same frame
par (new = TRUE)    

# infectious hosts over time
plot (I ~ time, data = output, type='l', ylim = c(0,1), col = 'red', ylab = '', axes = FALSE) 