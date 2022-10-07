
library(tidyverse)
library(sf)
library(sp)
library(stringr)
library(lubridate)
library(purrr)

# Notes on importing component data -------------------------------------------------

# Component data used for this code exceeds Github storage, so must be downloaded from separate locations. 
# Access each component data source in the following location:
# 1. ICS incidents: https://figshare.com/articles/dataset/ICS209-PLUS_Cleaned_databases/8048252/10
# 2. FIRED footprints: https://github.com/maxwellCcook/ics209-plus-fired/blob/main/data/spatial/mod/ics-fired/ics209plus_fired_events_cleaned_SHP.zip
# 3. MTBS footprints: Download MTBS Burned Areas Boundaries Dataset, https://www.mtbs.gov/direct-download
# 4. NHGIS spatial unit shapefiles: Download 2020 NHGIS spatial unit boundaries in shapefile format, https://usa.ipums.org/usa-action/variables/group 

# Import Data -------------------------------------------------------------

# Set local working directory where component data are downloaded and stored
setwd("")

# ICS incident data (dataframe)
ics <- read_csv("ics209-plus-wf_incidents_1999to2020.csv",
                col_types = cols(
                  PEAK_EVACUATIONS = col_double() # convert peak evacuations for clean reading
                )) %>%
  filter(INCTYP_ABBREVIATION != "RX") %>% # remove controlled burns
  dplyr::select(-(FIRED_ID)) 

# MTBS Data (sf object) - uses MTBS Burned Areas Boundaries dataset released on April 28, 2022
# MTBS details: https://www.mtbs.gov/articles/announcement/2020-data-release-april-28-2022
mtbs <- st_read("mtbs_perims_DD.shp") %>%
  st_make_valid() # complete geometries (needed for future joins)

# Make dataframe version of MTBS for faster spot checking
mtbs_df <- mtbs %>% as.data.frame() %>% dplyr::select(-(geometry))

# Import FIRED perimeters (sf object)
fired <- st_read("ics209plus_fired_events_cleaned_SHP/ics209plus_fired_events_cleaned.shp") %>%
  st_make_valid() %>% # complete geometries (needed for future joins)
  st_transform(crs = st_crs(mtbs)) # change coordinate system to match MTBS

# County boundaries (sf object)
counties <- st_read("nhgis0026_shapefile_tl2020_us_county_2020/US_county_2020.shp") %>%
  st_transform(crs = st_crs(mtbs)) # change coordinate system to match MTBS

# Remove initial attack fires ---------------------------------------------

# These specific fires were manually identified by L.S. and K.S.
ics2 <- filter(ics, !INCIDENT_ID %in% c("2005_OK-OKS-06-6086_EASTERN OKLAHOMA IA", 
                                        "2006_TX-TXS-06016_WEST TEXAS SUMMER FIRE",
                                        "2009_TX-TXS-009024_2009 SUMMER WILDFIRE RESPONSE",
                                        "2001_KY-KYS-02075_KY RIVER COMPLEX FEMA", 
                                        "2000_KY-KYS-01062_SE DISTRICT COMPLEX",
                                        "2001_KY-KYS-02074_EASTERN DIST COMPLEX FEMA",
                                        "1999_KY-KYS-00089_EAST DISTRICT COMPLEX",
                                        "2008_TX-TXS-88052_TEXAS WINTER FIRES 2008",
                                        "2007_GA-GAS-070008_MOP UP COMMAND",
                                        "2006_TX-TXS-06013_WEST TEXAS IA",
                                        "2006_CA-MVU-5394_BAJA CALIFORNIA-MEXICO",
                                        "2001_AL-ALS-01005_NORTH EAST AREA COMPLEX",
                                        "2008_CA-MNF-000579_JUNE ABCD MISC. COMPLEX",
                                        "2005_AR-BUP-000601_AR PARK GROUP SEVERITY",
                                        "2003_CA-MVU-9184_TECATE",
                                        "2006_CA-LMU-2401_LMU LIGHTNING SERIES",
                                        "2006_CA-LMU-2403_BATTALION 3 LCA",
                                        "2000_MT-NWS-105_NWLO IA COMPLEX",
                                        "2008_CA-BTU-007587_BTU LIGHTNING SUPPORT",
                                        "2011_CA-TCU-007951_TCU SEPTEMBER LIGHTNING",
                                        "2019_10727637_REGION 1 SEVERITY"
                                        )) 

# Remove double-counted complex sub-fires ---------------------------------

# Removing double-counted fires that were manually identified by M.C.
ics4 <- filter(ics2, !INCIDENT_ID %in% c("2017_7293073_REDWOOD VALLEY INCIDENT", # Mendocino Lake Complex
                                         "2017_7222114_SULFUR", 
                                         "2018_9090673_RANCH", # Mendocino Complex
                                         "2018_9257999_RIVER",
                                         "2017_7241051_TUBBS", # Central LNU Complex
                                         "2017_7290270_POCKET",
                                         "2017_7293146_NUNS",
                                         "2017_7212083_ATLAS", # Southern LNU Complex
                                         "2017_7211294_CASCADE", # NEU Wind Complex
                                         "2017_7211256_MCCOURTNEY",
                                         "2017_7211273_LOBO",
                                         "2008_CA-FFD-005449_PARKER ROAD", 
                                         "2015_2897183_TUNK BLOCK" # Okanogan Complex
))

# Add in MTBS linkages that were lost in filtering above
ics4$MTBS_FIRE_LIST[ics4$INCIDENT_ID == "2017_7293180_CENTRAL LNU COMPLEX"] <- c("'[CA3859812261820171009-TUBBS' 'CA3874312290720171009-POCKET' 'CA3838712252020171009-NUNS']") # Central LNU

# Add in MTBS linkages that were missing (identified manually)
ics4$MTBS_ID[ics4$INCIDENT_ID == "2004_TX-TXS-04012_EAST I-40"] <- c("TX3522310155920040219") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2005_OK-OKS-0540401_TEXANNA ROAD"] <- c("OK3544409543820051127") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2009_UT-FIF-000191_SAWMILL CANYON"] <- c("UT3918511210920090805") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2012_OK-OKS-42016_LUTHER FIRE"] <- c("OK3562709719920120803") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2014_402977_SAN DIEGO COMPLEX"] <- c("CA3312011716020140514") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2014_499672_ASSAYI LAKE"] <- c("NM3599610892020140613") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2014_499672_ASSAYI LAKE"] <- c("NM3599610892020140613")
ics4$MTBS_ID[ics4$INCIDENT_ID == "2015_2867226_MARBLE VALLEY"] <- c("WA4838311788820150814") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2015_2876454_REACH"] <- c("WA4779711998820150814") 
ics4$MTBS_FIRE_LIST[ics4$INCIDENT_ID == "2015_2920368_CLEARWATER/MUNICIPAL COMPLEX"] <- c("['ID4641811570620150811-MUSSELSHELL CREEK'
 'ID4642011560920150811-SNOWY SUMMIT' 'ID4641211555020150811-PETE FORKS'
 'ID4627411605820150811-KAMIAH GULCH']")
ics4$MTBS_ID[ics4$INCIDENT_ID == "2016_4363375_BLUE CUT TYPE 3"] <- c("CA3425911746120160816") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2017_7142555_LOGAN FIRE"] <- c("CO4078010283820170306") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2017_7211294_CASCADE"] <- c("CO4078010283820170306") 
ics4$MTBS_FIRE_LIST[ics4$INCIDENT_ID == "2017_7228286_SAPPHIRE COMPLEX"] <- c("['MT4665411363020170719-GOAT CREEK' 'MT4644711368920170713-LITTLE HOGBACK']")
ics4$MTBS_FIRE_LIST[ics4$INCIDENT_ID == "2017_7295260_WHITTIER"] <- c("['CA3456711995220170708-WHITTIER  CA-LBOR-001770']")
ics4$MTBS_ID[ics4$INCIDENT_ID == "2017_7332107_PONDEROSA"] <- c("CA3957512131620170829") 
ics4$MTBS_ID[ics4$INCIDENT_ID == "2017_9258165_THOMAS"] <- c("CA3442911910020171205") 

# Unnest fires with multiple MTBS ID's ------------------------------------

# Convert MTBS_FIRE_LIST into a list column
ics4$MTBS_FIRE_LIST <- lapply(strsplit(as.character(ics4$MTBS_FIRE_LIST),
                                       "[][']|,\\s*"), function(x) x[nzchar(x)])

# Unnest MTBS_FIRE_LIST column and remove residual rows that were created in unnesting
ics_unnest <- ics4 %>% unnest_longer(MTBS_FIRE_LIST) %>%
  filter(str_detect(MTBS_FIRE_LIST, '-') | is.na(MTBS_FIRE_LIST)) %>% # removes blank space rows created in unnesting
  separate(MTBS_FIRE_LIST, into = c("MTBS2", NA), sep = "-", remove = FALSE) %>% # clean final MTBS ID (remove fire name)
  mutate(MTBS_orig = ifelse(MTBS_ID == MTBS2, 1, 0)) # show where list MTBS is different than primary MTBS for spot checks later

# Make sure no observations were lost in unnesting and cleaning (nrow() should be zero below)
anti <- anti_join(ics4, ics_unnest, by = "INCIDENT_ID")
nrow(anti)

# Make MTBS_ID_FINAL column that integrates original MTBS ID and new list MTBS ID's
# Incidents with more than one unique MTBS ID are broken out into multiple corresponding rows
ics7 <- ics_unnest %>%
  mutate(MTBS_ID_FINAL = ifelse(nchar(MTBS2) < 7 | MTBS2 == MTBS_ID, MTBS_ID, MTBS2)) %>%
  mutate(MTBS_ID_FINAL = ifelse(is.na(MTBS_ID_FINAL), MTBS2, MTBS_ID_FINAL)) %>%
  mutate(MTBS_ID_FINAL = ifelse(is.na(MTBS_ID_FINAL), MTBS_ID, MTBS_ID_FINAL)) %>%
  mutate(MTBS_ID_FINAL = str_remove(MTBS_ID_FINAL, "\"")) %>% # remove a few remaining " from final MTBS ID column
  distinct(INCIDENT_ID, MTBS_ID_FINAL, FINAL_ACRES, .keep_all = TRUE) # remove small number of repeated rows

# Filter MTBS database to include only fires in ICS database (easier to wrangle smaller data)
ics7_w_mtbs <- filter(ics7, !is.na(MTBS_ID_FINAL)) 

mtbs2 <- left_join(mtbs, ics7_w_mtbs,
                   by = c("Event_ID" = "MTBS_ID_FINAL")) %>% 
  filter(!is.na(INCIDENT_ID)) %>% # return MTBS rows only when there is a match to ICS
  dplyr::select(Event_ID:High_T, INCIDENT_ID) 

# Spatial join (MTBS footprints & counties) ----------------------------------------------------

# Join MTBS footprints with ICS data (tabular join based on MTBS ID)
ics_join_mtbs_counties <- inner_join(mtbs2, ics7, by = c("Event_ID" = "MTBS_ID_FINAL")) %>%
  dplyr::select(-(irwinID:X1), -(Event_ID)) %>% # remove extraneous MTBS columns
  rename(INCIDENT_ID = INCIDENT_ID.y)

# Spatial join ICS-MTBS polygons with spatial units (counties)
ics_join_mtbs_counties2 <- st_join(ics_join_mtbs_counties, counties, left = TRUE) %>% # left = TRUE makes a left join
  dplyr::select(-(POO_LATITUDE:POO_LONGITUDE)) %>%
  mutate(MTBS_ID_FINAL = NA) %>%
  dplyr::select(-(LSAD:AWATER)) %>% # remove extraneous columns from counties data
  mutate(Spatial_Join_MTBS = 1) %>% # create variable marking that the spatial join came from MTBS
  as.data.frame() %>% # convert from sf to dataframe
  dplyr::select(-(geometry)) %>% # no longer need geometry details and they slow down processing, so remove
  distinct(INCIDENT_ID, GEOID, .keep_all = TRUE) # remove duplicates where different fire polygons of the same Incident ID fell within the same GEOID

# Spatial join (FIRED & counties) -----------------------------------------

# Join FIRED footprints with ICS data (tabular)
ics_join_fired_counties <- inner_join(fired, ics7, by = c("INCI_ID" = "INCIDENT_ID")) 

# Spatial join ICS-FIRED polygons with spatial units (counties)
ics_join_fired_counties2 <- st_join(ics_join_fired_counties, counties, left = TRUE) %>% # left = TRUE makes a left join
  dplyr::select(-(POO_LATITUDE:POO_LONGITUDE)) %>%
  dplyr::select(INCI_ID, FIRED_ID, GEOID, # select only variables for joining and FIRED-specific
                DISCOVERY_DATE, WF_CESSATION_DATE, FINAL_REPORT_DATE,
                tot_ar_ac, fsr_ac_dy, mx_grw_ac, mx_grw_dte) %>% 
  rename(INCIDENT_ID = INCI_ID) %>%
  mutate(Spatial_Join_FIRED = 1) %>% # create variable marking that the spatial join came from FIRED
  as.data.frame() %>% # convert from sf to dataframe
  dplyr::select(-(geometry)) %>% # no longer need geometry details and they slow down processing, so remove
  distinct(INCIDENT_ID, GEOID, .keep_all = TRUE) # remove duplicates where different fire polygons of the same Incident ID fell within the same GEOID

# Combine MTBS and FIRED counties -----------------------------------------

# Join MTBS and FIRED county designations 
# There should be a good amount of overlap between these two dataframes, as MTBS and FIRED will capture many of the same incidents
mtbs_fired_counties <- full_join(ics_join_mtbs_counties2, 
                                 ics_join_fired_counties2,
                                 by = c("INCIDENT_ID", "GEOID")) %>%
  mutate(DISCOVERY_DATE = coalesce(DISCOVERY_DATE.x, DISCOVERY_DATE.y)) %>% ## combine discovery dates from MTBS and FIRED
  mutate(WF_CESSATION_DATE = coalesce(WF_CESSATION_DATE.x, WF_CESSATION_DATE.y)) %>% ## combine cessation dates from MTBS and FIRED
  mutate(FINAL_REPORT_DATE = coalesce(FINAL_REPORT_DATE.x, FINAL_REPORT_DATE.y)) ## combine final report dates from MTBS and FIRED

# How many GEOID's were captured by MTBS, FIRED, or both?
mtbs_only_geoid_county <- mtbs_fired_counties %>% filter(Spatial_Join_MTBS == 1 & is.na(Spatial_Join_FIRED)) # only MTBS
fired_only_geoid_county <- mtbs_fired_counties %>% filter(Spatial_Join_FIRED == 1 & is.na(Spatial_Join_MTBS)) # only FIRED
mtbs_fired_both_geoid_county <- mtbs_fired_counties %>% filter(Spatial_Join_FIRED == 1 & Spatial_Join_MTBS == 1) # both

# Check to make sure that all observations are designated to MTBS, FIRED, or both (below should be TRUE)
nrow(mtbs_only_geoid_county) + nrow(fired_only_geoid_county) + nrow(mtbs_fired_both_geoid_county) == nrow(mtbs_fired_counties)

# Spatial join (POO & counties) ---------------------------------------------

# Filter to all remaining fire incidents that don't have MTBS or FIRED spatial information
ics8 <- ics7 %>% filter(!(INCIDENT_ID %in% mtbs_fired_counties$INCIDENT_ID)) %>% # remove incidents that are already processed with MTBS or FIRED
  mutate(Spatial_Join_POO = 1) # note that spatial details come from POO

# Make sure that all incidents are accounted for (below should = TRUE)
length(unique(ics8$INCIDENT_ID)) + length(unique(mtbs_fired_counties$INCIDENT_ID)) == length(unique(ics7$INCIDENT_ID))

# Convert lat/long to spatial points 
ics_points <- st_as_sf(ics8, 
                       coords = c("POO_LONGITUDE", "POO_LATITUDE"), 
                       crs = st_crs(counties), # set CRS to match spatial units
                       na.fail = FALSE)

# Join ICS points to spatial units 
ics_points_joined_counties <- st_join(ics_points, counties, left = TRUE) %>% # left = TRUE makes a left join
  dplyr::select(-(X1)) %>%
  mutate(MTBS_ID_FINAL = NA) %>%
  dplyr::select(-(LSAD:AWATER)) %>% # remove extraneous columns from counties data
  mutate(Spatial_Join_FIRED = NA,# create empty columns to match mtbs_fired_counties for rbind below
         Spatial_Join_MTBS = NA,
         tot_ar_ac = NA,
         fsr_ac_dy = NA,
         mx_grw_ac = NA,
         mx_grw_dte = NA) %>%
  #mutate(Spatial_Join_POO = 1) %>%
  distinct(INCIDENT_ID, GEOID, .keep_all = TRUE)

# Combine county designations from MTBS + FIRED and POO points ---------------------------------------------

# Combine ICS points and polygons 
ics_spatial_counties <- bind_rows(mtbs_fired_counties, ics_points_joined_counties) %>% 
  mutate(GEOID = as.character(GEOID)) %>%
  dplyr::select(-(geometry)) %>% # remove spatial details (no longer needed, slows down processing)
  mutate(Spatial_Data_Origin = case_when(Spatial_Join_MTBS == 1 & is.na(Spatial_Join_FIRED) ~ "MTBS", # create a variable identifying source of spatial join
                                  is.na(Spatial_Join_MTBS) & Spatial_Join_FIRED == 1 ~ "FIRED",
                                  Spatial_Join_MTBS == 1 & Spatial_Join_FIRED == 1 ~ "MTBS_and_FIRED",
                                  Spatial_Join_POO == 1 ~ "POO")) %>%
  dplyr::select(-(Spatial_Join_MTBS), -(Spatial_Join_FIRED), -(Spatial_Join_POO)) # remove component spatial data indicators 

# Check to make sure binding dataframes together worked properly (below should be TRUE)
nrow(mtbs_fired_counties) + nrow(ics_points_joined_counties) == nrow(ics_spatial_counties)

# Check for any duplicates, there should be no repeated Incident_ID-GEOID combinations (nrow(dup_check2) should be 0)
dup_check <- ics_spatial_counties %>% group_by(INCIDENT_ID, GEOID) %>% summarise(n_observations = n())
dup_check2 <- filter(dup_check, n_observations >1)
nrow(dup_check2)

# Compare number of total incidents captured in spatial product vs. total incidents in ICS
missing_counties <- filter(ics_spatial_counties, is.na(GEOID)) 
1 - (nrow(missing_counties) / length(unique(ics4$INCIDENT_ID))) # percent of ICS database with a complete spatial join

# Calculate % of incidents that overlap multiple counties -----------------

# Summarize total number of unique GEOID's per wildfire Incident ID
multiple_geoid_per_incident <- ics_spatial_counties %>%
  group_by(INCIDENT_ID) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs > 1) # Pull out only incidents with 2 or more county GEOID's

# Calculate proportion of incidents that cross multiple counties out of all incidents
length(unique(multiple_geoid_per_incident$INCIDENT_ID)) / length(unique(ics4$INCIDENT_ID))

# Calculate Quarters -----------------------------------------------

# Designate quarter and year (should increase the number of observations from ics_spatial_tracts, as fires across >1 quarter are broken into Fire-GEOID-Quarter)
ics_spatial_counties2 <- mutate(ics_spatial_counties, 
                              StartDate = as.character.POSIXt(DISCOVERY_DATE) %>% as.Date("%Y-%m-%d"), # rename start date, reformat to Date
                              StartQ = quarter(StartDate), # calculate start quarter
                              StartY = year(StartDate), # calculate start year
                              StartM = month(StartDate), # calculate start month
                              EndDate = as.character.POSIXt(coalesce(WF_CESSATION_DATE, FINAL_REPORT_DATE)) %>% as.Date("%Y-%m-%d"), # where cessation date is not available, use final report date
                              EndQ = quarter(EndDate), # calculate end quarter
                              EndY = year(EndDate), # calculate end year
                              EndM = month(EndDate), # calculate end month
                              End_DOY = yday(EndDate), # julian end doy
                              FireInterval = interval(StartDate, EndDate), # create interval
                              Q1_days = time_length(lubridate::intersect(interval(ymd(paste(StartY, "01", "01", sep="-")), ymd(paste(StartY, "03", "31", sep="-"))), FireInterval), "day"), # intersection of fire interval with quarter intervals
                              Q2_days = time_length(lubridate::intersect(interval(ymd(paste(StartY, "04", "01", sep="-")), ymd(paste(StartY, "06", "30", sep="-"))), FireInterval), "day"),
                              Q3_days = time_length(lubridate::intersect(interval(ymd(paste(StartY, "07", "01", sep="-")), ymd(paste(StartY, "09", "30", sep="-"))), FireInterval), "day"),
                              Q4_days = time_length(lubridate::intersect(interval(ymd(paste(StartY, "10", "01", sep="-")), ymd(paste(StartY, "12", "31", sep="-"))), FireInterval), "day"),
                              Q1_2_days = time_length(lubridate::intersect(interval(ymd(paste(StartY +1, "01", "01", sep="-")), ymd(paste(StartY +1, "03", "31", sep="-"))), FireInterval), "day"), # include quarters from year following start of fire
                              Q2_2_days = time_length(lubridate::intersect(interval(ymd(paste(StartY +1, "04", "01", sep="-")), ymd(paste(StartY +1, "06", "30", sep="-"))), FireInterval), "day"), # in case the fire burns across multiple years
                              Q3_2_days = time_length(lubridate::intersect(interval(ymd(paste(StartY +1, "07", "01", sep="-")), ymd(paste(StartY +1, "09", "30", sep="-"))), FireInterval), "day"),
                              Q4_2_days = time_length(lubridate::intersect(interval(ymd(paste(StartY +1, "10", "01", sep="-")), ymd(paste(StartY +1, "12", "31", sep="-"))), FireInterval), "day"),
                              Total_Duration_Days = interval(StartDate, EndDate) %>% time_length(unit = "day")) %>% # calculate total duration days of fire interval
  mutate(Total_Duration_Days2 = rowSums(across(Q1_days:Q4_days), na.rm = T)) %>%
  pivot_longer(cols = Q1_days:Q4_2_days, # combine all quarter duration days into two columns, Quarter (which quarter), and Q_Duration_Days (how many days in that quarter)
               values_drop_na = TRUE,
               values_to = "Q_Duration_Days",
               names_to = "Quarter") %>%
  mutate(Total_Duration_Days = ifelse(Total_Duration_Days == 0, 1, Total_Duration_Days)) %>% # switch zeroes (fires that last one day) to "1" for proportion calculation
  mutate(Total_Duration_Days2 = ifelse(Total_Duration_Days2 == 0, 1, Total_Duration_Days2)) %>% # 
  mutate(Q_Duration_Days = ifelse(Q_Duration_Days == 0, 1, Q_Duration_Days)) %>%
  mutate(Q_Proportion = Q_Duration_Days/Total_Duration_Days2) %>% # calculate proportion of burn days within each quarter
  mutate(Quarter = replace(Quarter, Quarter == "Q1_days",  values = 1), # replace names of quarters with numeric values
         Quarter = replace(Quarter, Quarter == "Q2_days",  values = 2),
         Quarter = replace(Quarter, Quarter == "Q3_days",  values = 3),
         Quarter = replace(Quarter, Quarter == "Q4_days",  values = 4), 
         Quarter = replace(Quarter, Quarter == "Q1_2_days",  values = 1.5), # replace names of quarters with numeric values
         Quarter = replace(Quarter, Quarter == "Q2_2_days",  values = 2.5),
         Quarter = replace(Quarter, Quarter == "Q3_2_days",  values = 3.5),
         Quarter = replace(Quarter, Quarter == "Q4_2_days",  values = 4.5)) %>%
  mutate(Year = ifelse(Quarter %in% c(1, 2, 3, 4), StartY, 1)) %>% # if quarter is within the 1-4 of the same year, use StartY as the year designation
  mutate(Year = ifelse(Quarter %in% c(1.5, 2.5, 3.5, 4.5), EndY, Year)) %>% # if the quarters are from the year following Start Year, select EndY as year designation
  mutate(Quarter = ifelse(Quarter == 1.5, 1, Quarter)) %>% # convert quarters from second year to standard categories, 1, 2, 3, 4
  mutate(Quarter = ifelse(Quarter == 2.5, 2, Quarter)) %>%
  mutate(Quarter = ifelse(Quarter == 3.5, 3, Quarter)) %>%
  mutate(Quarter = ifelse(Quarter == 4.5, 4, Quarter)) %>%
  mutate(cross_quarter_indicator = ifelse(StartQ == EndQ & StartY == EndY, 0, 1)) %>% # make indicator for fires that cross over multiple quarters
  mutate(cross_year_indicator = ifelse(StartY != EndY, 1, 0)) %>% # make indicator for fires that cross years
  dplyr::select(-(StartDate), # Remove extraneous
                -(EndDate),
                -(StartQ),
                -(EndQ),
                -(StartY),
                -(EndY),
                -(StartM),
                -(Q_Duration_Days),
                -(Q_Proportion),
                -(Total_Duration_Days2),
                -(Total_Duration_Days2),
                -(FireInterval),
                -(End_DOY)) 

# Create final dataframe that removes extraneous columns
ics_spatial_counties3 <- ics_spatial_counties2 %>%
  dplyr::select(GEOID, INCIDENT_ID, FIRED_ID, 
                Quarter, Year, Spatial_Data_Origin)

# Save final dataframe
write_csv(ics_spatial_counties3, "ics_spatial_counties.csv")

# Calculate % of incidents that overlap multiple quarters -----------------

# Note that this code does not change at all between county, tract, or CBG-level data (it's not based on spatial units)
# Summarize total number of unique quarters per wildfire Incident ID
multiple_quarter_per_incident <- ics_spatial_counties2 %>%
  dplyr::select(INCIDENT_ID, cross_quarter_indicator) %>%  # select only two relevant columns
  distinct() %>% # remove duplicated records (reverts back to incident level from incident-GEOID-quarter)
  filter(cross_quarter_indicator == 1) # Pull out only incidents with 2 or more quarters

# Calculate proportion of incidents that cross multiple quarters out of all incidents
length(unique(multiple_quarter_per_incident$INCIDENT_ID)) / length(unique(ics4$INCIDENT_ID))




