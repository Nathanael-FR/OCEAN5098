# setwd("C:/Users/natha/Documents/R_NTU_2023/Course 3/Practice 2.2")

library(tidyr)
library(dplyr)

rairuoho<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt',
                     header=T,
                     sep="\t")

rairuoho_formated <- rairuoho %>%
  pivot_longer(cols=day3:day8,names_to = "day", values_to = "length") %>%
  mutate(treatment = ifelse(treatment=="nutrient","enriched",treatment),
         day = factor(day,levels = c("day3","day4","day5","day6","day7","day8"))) %>%
  unite(c("spatial1","spatial2"),col="Spatial",sep = "_") %>%  # Function found in the cheat sheet p17
  select(-row,-column) # mutate(row=NULL,column=NULL) also works


# Another method 
#
# rairuoho$treatment <- ifelse(rairuoho$treatment=="nutrient","enriched","water")
# rairuoho$row = NULL
# rairuoho$column = NULL
# rairuoho_formated_bis <- rairuoho %>%
#   pivot_longer(cols = day3:day8, names_to = "day", values_to = "length") %>%
#   mutate(day = factor(day, levels = c("day3","day4","day5","day6","day7","day8"))) %>%
#   unite(c("spatial1","spatial2"),col="Spatial",sep = "_")
# 
#   


# test <- rairuoho %>% pivot_longer(cols=c(spatial1,spatial2),names_to = "Spatial")







