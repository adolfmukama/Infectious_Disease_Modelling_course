#Load packages
library(deSolve)
library(reshape2)
library(ggplot2)
'In this etivity, we are trying to simulate an outbreak of a new infectious disease that our population of 10^6
  people has not been exposed to before. This means that we are starting with a single case, everyone else is susceptible to the disease, and no one is yet immune or recovered. This can for example reflect a situation where an infected person introduces a new disease into a geographically isolated population, like on an island, or even when an infections "spill over" from other animals into a human population. In terms of the initial conditions for our model, we can define: S = 10 6
 -1 = 999999, I = 1 and R = 0.'

#Function

cohort_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)),{
    
    dS = (-1 * h) * S
    
    dI = (h * S) - (y * I)
    
    dR = y * I
    return(list(c(dS,dI,dR)))
  })
}

times <- seq(from =0, to = 60, by = 1)
y <- c(S = 999999,
       I = 1,
       R = 0)

paras <- c(h = 0.2,
           y = 0.1)

output <- as.data.frame(ode(y,times,cohort_model,paras))
output
output_long <- melt(as.data.frame(output), id = "time")

output_long
ggplot(data = output_long,
       aes(x = time, y = value, 
       colour = variable,
       group = variable)) +
  geom_line() +
  xlab("time(days)") +
  ylab("Number of people")+
  labs(clour = "compartment")
