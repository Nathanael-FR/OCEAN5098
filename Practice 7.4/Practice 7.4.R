setwd("C:/Users/natha/Documents/R_NTU_2023/Course 9/Practice 7.4/")
rm(list = ls())

library(tidyr)
library(dplyr)

rairuoho<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt',
                     header=T,
                     sep="\t")

rairuoho_formated <- rairuoho %>%
  pivot_longer(cols=day3:day8,names_to = "day", values_to = "length") %>%
  mutate(treatment = ifelse(treatment=="nutrient","enriched",treatment),
         day = factor(day,levels = c("day3","day4","day5","day6","day7","day8"))) %>%
  unite(c("spatial1","spatial2"),col="Spatial",sep = "_") %>%  
  select(-row,-column, -Spatial,) 



my_t_test <- function(data, day, alpha) {
  cat("\n")
  cat("Null Hypothesis : The 2 population have the same mean, the difference of treatment has no effect at",day)
  cat("\n")
  
  group1 <- data[data$treatment == "enriched" & data$day == day, "length"]
  group2 <- data[data$treatment == "water" & data$day == day, "length"]
  
  mean1 <- mean(group1$length)
  mean2 <- mean(group2$length)
  
  var1 <- var(group1$length)
  var2 <- var(group2$length)
  
  n1 <- length(group1$length)
  n2 <- length(group2$length)
  
  t_value <- abs(mean1-mean2) / sqrt((var1/n1)+(var2/n2))
  
  df <- n1 + n2 - 2

  p_value <- 2*(1-pt(t_value, df))
  
  cat("data : Length of the germinate at",day,"\n")
  cat("t =",t_value,"df =",df,"p-value",p_value,"\n")
  cat("Mean of group1 (enriched) :",mean1,"Mean of group2 (water)",mean2,"\n")
  if ( p_value > alpha ) {
    cat(day, "Null Hypothesis accepted with a risk of",alpha*100,"% :\n
          The difference of treatment has not a significant impact on the length of species
          at \n")
    
  }
  else {
    cat(day,"Alternative Hypothesis accepted with a risk of",alpha*100,"% :\n 
        The difference of treatment has a significant impact on the length of the species \n")
  }
  
}

days <- levels(rairuoho_formated$day)

alpha = 0.05
for (day in days) {
  t_test_result <- my_t_test(rairuoho_formated, day, alpha)
  
}
