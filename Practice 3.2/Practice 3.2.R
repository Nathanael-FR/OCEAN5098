#setwd("C:/Users/natha/Documents/R_NTU_2023/Course 4/Practice 3.2")
rm(list=ls())


before_diet <- c(104,95,87,77,112)
after_diet <- c(96,91,81,75,118)
df <- data.frame(before_diet,after_diet) 
row.names(df) <- c("subject_1","subject_2","subject_3","subject_4","subject_5")
row

library(dplyr)
library(tidyr)

# Reformat the data
df_reformated <- df %>%
    tibble::rownames_to_column(var = "Subject") %>%
    pivot_longer(cols = c(before_diet,after_diet),
               names_to = "time",
               values_to = "weight")

df_reformated$weight <- as.double(df_reformated$weight)
df_reformated$time <- as.factor(df_reformated$time)

# Check data types
df_reformated$time
typeof(df_reformated$time)

typeof(df_reformated$weight)

# Get patients ID
Patients <- row.names(df)

# Get the weight loss for each of them
Weight_loss <- df_reformated %>%
  group_by(Subject) %>%
  summarise(weight_loss = (weight[time == "before_diet"] - weight[time == "after_diet"]) / weight[time == "before_diet"] * 100,
            .groups = 'drop')

Weight_loss <- Weight_loss$weight_loss

# Create a list to gather everything
BUBBLE_DIET <- list(df_reformated,
                    WEIGHT_LOSS = list(
                      Patients,
                      Weight_loss,
                      data.frame(Patients,Weight_loss)),
                    Message = "I actualy like R, but sometimes I wonder if some operations
                    would be easier to do on a wide df than on a long one. Like the weight_loss%, for me
                    it was more instinctive to get it on the first df. See the example below :"
                    )



# Second approach

# df$before_diet <- as.double(df$before_diet)
# df$after_diet <- as.double(df$after_diet)
# df <- df %>%
#   mutate(Weight_loss = (before_diet - after_diet)/before_diet*100)
#   
# Weight_loss2 <- df$Weight_loss


