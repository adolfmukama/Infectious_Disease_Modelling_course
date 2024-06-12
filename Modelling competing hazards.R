'Previously, we modelled an infected cohort in R. In this exercise, 
the focus is on adding disease-induced mortality to this model in order 
to explore the concept of competing hazards as introduced in the video. 
Using this model, we will also calculate the case fatality ratio'

library(deSolve)
library(reshape2)
library(ggplot2)

hazards <- function(time,state,parameters){
  with(as.list(c(state, parameters)), {
    dI = (-y * I) - (u * I)
    
    dR = y * I
    
    dM = u * I
    
    return(list(c(dI,dR,dM)))
    
  })
}

times <- seq(0, 28, by = 1 )
paras <- c(y = 0.1,
           
           u = 0.2)
y <- c(I = 1000000,
       R = 0,
       M = 0)

result <- ode(y, times,hazards, paras)
result1 <- as.data.frame(result)
print(result1)

# plot for the model

output_long <- melt(as.data.frame(result1), id = "time") 
output_long

ggplot(data = output_long,   # specify object containing data to plot
       aes(x = time, 
           y = value, 
           colour = variable, 
           group = variable)) +       # assign columns to axes and groups
  geom_line() +                       # represent data as lines
  xlab("Time (days)")+                # add label for x axis
  ylab("Number of people") +          # add label for y axis
  labs(colour = "Compartment")        # add legend title

#proportion of initially infected that died before recovering

(result1$M[result1$time == 28]/((result1$M[result1$time == 28]) +(result1$I[result1$time == 28]) + (result1$R[result1$time == 28]))) * 100

#now use the competing hazards formula given in the video lecture to calculate the case fatality rate

case_fatality_rate <- (0.2/(0.2 + 0.1)) * 100
case_fatality_rate


