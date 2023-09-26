setwd("C:/Users/natha/Documents/R_NTU_2023/Project")
rm(list=ls())

# Import the data
df <- read.csv("prod_par_filière_mensuel.csv",sep = ";")
dim(df)


# Import the package
library(dplyr)
library(tidyr)
library(ggplot2)


# Overview of the data
summary(df)   

# Convert the data in the correct format
df$Valeur..TWh.<- as.numeric(gsub(",", ".", df$Valeur..TWh.))
df$Filière <- as.factor(df$Filière)

# Group the data by year
yearly_df <- df %>%
  separate(col = Date,sep = "-",into = c("Year","Month")) %>%
  select(-Month) %>%
  group_by(Year,Filière) %>%
  summarise('Production' = sum(Valeur..TWh.))

# "Production totale" is an implicit value since we got all the types of production
# so we remove it
yearly_df <- yearly_df[yearly_df$Filière !="Production totale",]

# Colors code for productions types:
custom_colors <- c(
  "Eolien" = "darkslategray1",
  "Hydraulique" = "cornflowerblue",
  "Nucléaire" = "brown1",
  "Solaire" = "yellow",
  "Thermique fossile" = "orange",
  "Thermique renouvelable et déchets" = "chartreuse3"
)

# Bar graph of the production throughout the years
bar_graph <- ggplot(yearly_df, aes(x = Year, y = Production))+
  geom_col(aes(fill=Filière)) +
  labs(title = "Production d'énergie par filière",
       x = "Année",
       y = "Production éléctrique (TWh)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = custom_colors) 

# Overlook upon 2022
df_2022 <- yearly_df[yearly_df$Year==2022,]

# Add proportion row to create the next graph
df_2022 <- df_2022 %>%
  arrange(desc(Production)) %>%
  mutate(prop = Production / sum(df_2022$Production) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Coord graph of the productio during 2022
camembert <- ggplot(df_2022, aes(x="",y=Production, fill=Filière)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Production d'énergie par filière") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = custom_colors) 

  





