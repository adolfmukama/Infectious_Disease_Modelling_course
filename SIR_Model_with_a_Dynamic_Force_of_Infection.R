#Load packages
library(deSolve)
library(reshape2)
library(ggplot2)

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