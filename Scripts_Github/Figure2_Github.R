library(tidyverse)
library(sf)
library(ggthemes)

# Code to produce Figure 2 of Camp Fire burn footprints and corresponding census tracts

# Import Data -------------------------------------------------------------

# Set local working directory
setwd("")

# Import ICS tracts (df object)
ics_tracts <- read_csv("ics_spatial_tracts.csv")

# Tract boundaries (sf object)
tracts <- st_read("Snhgis0024_shapefile_tl2020_us_tract_2020/US_tract_2020.shp") %>%
  st_transform(crs = st_crs(mtbs)) # change coordinate system to match MTBS

# MTBS Data (sf object) - uses MTBS Burned Areas Boundaries dataset released on April 28, 2022
# MTBS details: https://www.mtbs.gov/articles/announcement/2020-data-release-april-28-2022
mtbs <- st_read("mtbs_perims_DD.shp") %>%
  st_make_valid() # complete geometries 

# Import FIRED perimeters (sf object)
fired <- st_read("ics209plus_fired_events_cleaned_SHP/ics209plus_fired_events_cleaned.shp") %>%
  st_make_valid() %>% # complete geometries (needed for future joins)
  st_transform(crs = st_crs(mtbs)) # change coordinate system to match MTBS

# Create map image --------------------------------------------------------

# Camp Fire
camp <- filter(mtbs, Event_ID == "CA3982012144020181108") 
final_camp <- filter(ics_tracts, INCIDENT_ID == "2018_9220077_CAMP") 
final_camp_sf <- dplyr::filter(tracts, GEOID %in% final_camp$GEOID)
final_camp_fired <- dplyr::filter(fired, INCI_ID == "2018_9220077_CAMP") 

ggplot() +
  geom_sf(data = final_camp_sf, lwd = .3) + # tracts from final dataset
  geom_sf(data = camp, fill=alpha("red",0.4), color = NA) + # MTBS footprint
  geom_sf(data = final_camp_fired, fill=alpha("blue",0.4), color = NA) + # FIRED footprint
  theme_map() 
