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

result2 <- ggplot2(data = result)
result1 <- result + geom_line(aes(x = time, y = I), color = "blue")