setwd("C:/Users/natha/Documents/R_NTU_2023/Project")
rm(list=ls())

library(leaflet)
library(sf)
library(tidyr)
library(dplyr)
library(stringi)
library(htmltools)


df <- read.csv("data/registre-national-installation-production-stockage-electricite-agrege_ODRE.csv",sep = ";")
col <- c("departement","filiere","puismaxinstallee" )
df <- df[,col]

# Encoding(df$filiere)<-'latin1'
# df$filiere <- stri_trans_general(df$filiere, "Any-Latin; Latin-ASCII")


geo_data = st_read("données geospatiales/departements/departement.shp")

geo_data <- st_simplify(geo_data, dTolerance = 70)

# Get rid of the accent (�) 

Encoding(df$departement)<-'latin1'
df$departement <- gsub(" ", "-", df$departement)
geo_data$nom <- gsub(" ", "-", geo_data$nom)
df$departement <- stri_trans_general(df$departement, "Latin-ASCII")
geo_data$nom <- stri_trans_general(geo_data$nom, "Latin-ASCII")


df_puis <- df %>%
  group_by(departement) %>%
  summarise(SumPowMaxInstalled = sum(coalesce(puismaxinstallee, 0)))



geo_data_with_pow <- left_join(geo_data, df_puis, by = c("nom" = "departement"))

color_palette <- colorNumeric(palette = c("lightyellow", "yellow", "orange", "red", "darkred"),
                           domain = geo_data_with_pow$SumPowMaxInstalled,
                           na.color = "grey")


# geo_data_with_pow <- geo_data_with_pow %>%
#   na.omit(geo_data_with_pow) %>%
#   mutate(label = paste("Departement:", nom, "Max.Power Installed:", SumPowMaxInstalled, sep = "<br>"))


map_department <- leaflet(data = geo_data_with_pow) %>%
  # addProviderTiles(provider = providers$OpenStreetMap.France) %>%
  setView(lng = -2.3508, lat = 46.8566, zoom = 5 ) %>%
  addPolygons(fillColor = ~color_palette(SumPowMaxInstalled),
              fillOpacity = 0.7,
              color = "black",
              weight = 1,
              # label = ~paste(label)) %>%
              label = ~paste("Department :",nom, "| Max.Power Installed :", SumPowMaxInstalled)) %>%
  addLegend( 
    pal = color_palette,
    values = ~SumPowMaxInstalled,
    title = "Max. Power Installed (MWh) (31/08/2023) </br> Source : Opendata Réseaux-Energies",
    position = "bottomleft",
    na.label = "Indisponible",
    )  %>%
addMarkers(lng = 2.2151292829731215, lat = 48.819804719338165, label = "My Home")
# A marker for where I'm born

# I couldn't display a title and jump a line between "Departement:"nom and "Max.Power Installed :", SumPowMaxInstalled
# I tried with mutate and paste to create a new column with the elements spaced with the html element </br> but 
# It didn't work

# geo_data_with_pow <- geo_data_with_pow %>%
#   na.omit(geo_data_with_pow) %>%
#   mutate(label = paste("Departement:", nom, "Max.Power Installed:", SumPowMaxInstalled, sep = "</br>"))

#  or  mutate(label = paste( c("Departement:", nom,), c("Max.Power Installed:", SumPowMaxInstalled), sep = "</br>"))
#