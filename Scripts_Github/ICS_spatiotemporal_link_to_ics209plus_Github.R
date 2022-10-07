
library(tidyverse)

# This code demonstrates linking the spatiotemporal version of the ICS-209-PLUS to the primary ICS-209-PLUS files
# This procedure allows researchers to link wildfire metrics from ICS-209-PLUS to the counties/tracts/block groups in which the fire occurred

# Import data -------------------------------------------------------------

# Set local working directory where data are stored
setwd("")

# ICS-209-PLUS primary incident table data
ics <- read_csv("") 

# ICS-209-PLUS spatiotemporal linkage (e.g. county, tract, or census block group level data)
ics_st <- read_csv("") # 

# Join data ---------------------------------------------------------------

# Join fire variables from ICS-209-PLUS primary incident table data to ICS-209-PLUS spatiotemporal
# Resulting data is observed at the GEOID-Incident_ID-Quarter-Year level
ics_joined <- left_join(ics_st, ics, by = c("INCIDENT_ID"))



