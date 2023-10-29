# setwd("C:/Users/natha/Documents/R_NTU_2023/Course 8")

library(animation)
library(ggplot2)

grow <- function(start_1, start_2, gen) {
  num_gen <- gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  generation <- 1:num_gen
  growth.rate <- 1.2
  K1 <- 100
  K2 <- 120
  a12 <- 0.8
  a21 <- 0.8
  N1[1] <- start_1
  N2[1] <- start_2
  for (i in 2:num_gen) {
    N1[i] <- N1[i - 1] + (growth.rate * N1[i - 1] * (K1 - N1[i - 1] - (a12 * N2[i - 1])) / K1)
    N2[i] <- N2[i - 1] + (growth.rate * N2[i - 1] * (K2 - N2[i - 1] - (a21 * N1[i - 1])) / K2)
  }
  if (N1[1] > N2[1]) {
    plot(N1 ~ generation, type = "b", ylim = c(0, max(K1, K2)), ylab = "N1", xlab = "generation")
    text(4, 110, "Species 1 alone")
  } else if (N1[1] < N2[1]) {
    plot(N2 ~ generation, type = "b", ylim = c(0, max(K1, K2)), ylab = "N2", xlab = "generation", col = "red")
    text(4, 110, "Species 2 alone")
  } else {
    plot(N1 ~ generation, type = "b", ylim = c(0, max(K1, K2)), ylab = "N1 and N2 ", xlab = "generation")
    lines(N2 ~ generation, type = "b", col = "red")
  }
}

saveGIF({
  for (i in seq(2,30,1)){
    
    par(mar=c(4,4,1,1),mfrow=c(3,1),las=1)
    
    grow(1, 0, i)
    text(4, 110, "Species 1 alone")
    
    grow(0, 1, i)
    text(4, 110, "Species 2 alone")
    
    
    grow(1, 1, i)
    text(6,110,"Both Species competing")
  }},
  interval = 0.1)
