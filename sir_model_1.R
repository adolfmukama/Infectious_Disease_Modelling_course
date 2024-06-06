#install.packages("deSolve")
#install.packages("ggplot2")
#install.packages("reshape2")
#library(deSolve)
#library(ggplot2)
#library(reshape2)

# First simple SiR model

# Research qn -- 
#--- how long it takes for a cohort of infected people to recover ---


funct <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
  dI = -alpha* I
    
  dR = alpha* I
  
  return(list(c(dI, dR)))
  })
}
y <- c(I = 1000000, R = 0)
times <- seq(0,28, by = 1)
paras <- c(alpha = 0.5)
result <- (ode(y,times,funct,paras))
print(result)

graph1 <- ggplot(data = result)
graph1 <- graph1 + geom_line(aes(x = time, y = R), col = "red")
graph1 <- graph1 + geom_line(aes(x = time, y = I), col = "blue")
graph1 <- graph1 + labs (x = "Time (days)", y = " Number of people")
graph1

'Based on the output, how many people 
have recovered after 4 weeks? 
What proportion of the total 
population does this correspond 
to?'
result <- as.data.frame(result)
result$R[result$time == 28]/(result$I[result$time == 28]+ result$R[result$time == 28])
result[result$time == 7,]
