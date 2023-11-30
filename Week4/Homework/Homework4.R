library(sf)
library(here)
library(readxl)
library(janitor)
library(tidyverse)
library(tmap)

# read in composite data csv file; have a look at it
gender_inequal_index_composite <- read.csv(here::here("Homework", "HDR21-22_Composite_indices_complete_time_series.csv"))%>%
  clean_names()
print(gender_inequal_index_composite)

#read in shape file; have a look at it
world <- sf::st_read(here::here("Homework", "World_Countries", "World_Countries_Generalized.shp")) 
print(world)

# subtract the gender inequality index (gii) for 2010 from gii_2019 and add it to a new column
gender_inequal_index <- gender_inequal_index_composite %>%
  mutate(propdiff_gii_2010_2019=(((gii_2019/gii_2010)*100)-100))

#join the spatial and csv data
joined_data <- world %>% 
  clean_names(.) %>%
  left_join(., 
            gender_inequal_index,
            by = c("country" = "country"))

# Attempt at map for % Change in GII 2010-2019
tm_shape(joined_data) + 
  tm_polygons("propdiff_gii_2010_2019", 
              # style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Change in Gender Inequality Index 2010 - 2019",
              alpha = 0.5) + 
  # tm_compass(position = c("left", "bottom"),type = "arrow") + 
  # tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "% DIfference in Gender Inequality Index 2010 - 2019", 
            legend.position = c("left", "bottom"))

#quick thematic map for % Change in GII 2010-2019
qtm(joined_data, 'propdiff_gii_2010_2019')
