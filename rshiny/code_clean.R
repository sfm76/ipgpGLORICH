# Packages
library(tidyverse)
library(dbplyr)
library(ggplot2)

# Reading CSV files (from GLORICH data) into data frames
hydro_data <- read.csv("hydrochemistry.csv")
locs <- read.csv("sampling_locations.csv")
catchment <- read.csv("catchment_properties.csv")

###### Data Cleaning and Formatting ######
df <- hydro_data %>%
  # Converting datetime format
  mutate(RESULT_DATETIME=as.Date(RESULT_DATETIME, format='%d/%m/%Y %H:%M:%S')) %>%
  # Grouping by station ID
  group_by(STAT_ID) %>%
  # Calculating start and end times
  mutate(start_time=min(RESULT_DATETIME, na.rm = TRUE),
         end_time=max(RESULT_DATETIME, na.rm = TRUE)) %>%
  ungroup() %>%
  # Selecting specific columns
  select(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3, HCO3_vrc, Alkalinity,
         Alkalinity_vrc, CO3, Ca, Mg, Na, K, SiO2) %>%
  # Calculating duration in years
  mutate(duration=abs(difftime(start_time, end_time, units="weeks"))/52) %>%
  mutate(duration=gsub(" weeks", "", duration)) 


###### Data Aggregation for Bicarbonate ###### 
sampling_hco3 <- df %>%
  # Grouping data by station ID
  group_by(STAT_ID) %>%
  # Filtering out rows with missing HCO3 values
  filter(!is.na(HCO3)) %>%
  # Calculating average for each station
  summarize(
    RESULT_DATETIME=RESULT_DATETIME, 
    start_time=start_time, 
    end_time=end_time,
    HCO3=HCO3, 
    HCO3_avg = mean(HCO3, na.rm = TRUE)) %>%
  ungroup() %>%
  # Taking daily averages for stations that have multiple HCO3 values per day
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3_avg) %>%
  summarize(HCO3 = mean(HCO3)) %>%
  ungroup() %>%
  # Calculating start value, end value, and duration
  group_by(STAT_ID) %>%
  summarize(
    start_time=start_time, 
    end_time=end_time,
    HCO3_start=HCO3[RESULT_DATETIME == min(start_time)],
    HCO3_end=HCO3[RESULT_DATETIME == max(end_time)],
    HCO3_avg=HCO3_avg,
    duration=as.numeric(max(end_time) - min(start_time))) %>%
  # Changing duration to be in years
  mutate(duration=duration/365) %>%
  # Calculating the change in HCO3 concentration over the period (change in micromole per L)
  mutate(HCO3_dif=HCO3_end - HCO3_start) %>% 
  # Calculating the slope of change in HCO3 concentration over time (micromole per L per year)
  mutate(HCO3_slope=HCO3_dif/duration) %>% 
  # Calculating the relative change in HCO3 concentration as a percentage of the initial value
  mutate(HCO3_rc=(HCO3_dif/HCO3_start)*100) %>%
  # Removing duplicate rows and keeping distinct station summaries
  distinct() %>%
  # Filtering out stations with only one HCO3 value ( ~1000 stations with only one value)
  filter(duration!=0) %>%
  # Merging with location data based on station ID
  merge(locs, by="STAT_ID") %>%
  # Converting Latitude and Longitude to numeric values from strings
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude))
# Save to csv to import into QGIS
write.csv(sampling_hco3, "samples_hco3.csv")
####################################### 
# code below follows same format as above but respective to alkalinity and the major elements

####### Alkalinity #########
sampling_alkalinity <- df %>%
  group_by(STAT_ID) %>%
  filter(!is.na(Alkalinity)) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, Alkalinity=Alkalinity, Alkalinity_avg = mean(Alkalinity, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, Alkalinity_avg) %>%
  summarize(Alkalinity = mean(Alkalinity)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(start_time=start_time, 
            end_time=end_time,
            Alk_start=Alkalinity[RESULT_DATETIME == min(start_time)],
            Alk_end=Alkalinity[RESULT_DATETIME == max(end_time)],
            Alkalinity_avg=Alkalinity_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(Alk_dif=Alk_end-Alk_start) %>% # change in micromole per L
  mutate(Alk_slope=Alk_dif/duration) %>% # micromole per L per year 
  mutate(Alkalinity_rc=(Alk_dif/Alk_start)*100) %>% # percentage of OG value
  distinct() %>%
  # removing stations that only recorded one alkalinity value ( ~4000 stations )
  filter(duration!=0) %>%
  merge(locs, by="STAT_ID") %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude))

####### Carbonate Ion ####### 
sampling_carb <- df %>%
  group_by(STAT_ID) %>%
  filter(!is.na(CO3)) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, CO3=CO3, CO3_avg = mean(CO3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  # get daily averages for locations that have multiple per day
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, CO3_avg) %>%
  summarize(CO3 = mean(CO3)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(start_time=start_time, 
            end_time=end_time,CO3_start=CO3[RESULT_DATETIME == min(start_time)],
            CO3_end=CO3[RESULT_DATETIME == max(end_time)],
            CO3_avg=CO3_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(CO3_dif=CO3_end-CO3_start) %>% # change in micromole per L
  mutate(CO3_slope=CO3_dif/duration) %>% # micromole per L per year 
  mutate(CO3_rc=(CO3_dif/CO3_start)*100) %>% # percentage of OG value
  distinct() %>%
  # removing stations that only recorded one CO3 value ( ~1000 stations )
  filter(duration!=0)

####### Magnesium ########
sampling_mg <- df %>%
  group_by(STAT_ID) %>%
  filter(!is.na(Mg)) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, Mg=Mg, Mg_avg = mean(Mg, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  # get daily averages for locations that have multiple per day
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, Mg_avg) %>%
  summarize(Mg = mean(Mg)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(start_time=start_time, 
            end_time=end_time,Mg_start=Mg[RESULT_DATETIME == min(start_time)],
            Mg_end=Mg[RESULT_DATETIME == max(end_time)],
            Mg_avg=Mg_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(Mg_dif=Mg_end-Mg_start) %>% # change in micromole per L
  mutate(Mg_slope=Mg_dif/duration) %>% # micromole per L per year 
  mutate(Mg_rc=(Mg_dif/Mg_start)*100) %>% # percentage of OG value
  distinct() %>%
  # removing stations that only recorded one CO3 value ( ~1000 stations )
  filter(duration!=0) %>%
  merge(locs, by="STAT_ID") %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude))
  
####### Calcium ####### 
sampling_ca <- df %>%
  group_by(STAT_ID) %>%
  filter(!is.na(Ca)) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, Ca=Ca, Ca_avg = mean(Ca, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  # get daily averages for locations that have multiple per day
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, Ca_avg) %>%
  summarize(Ca = mean(Ca)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(start_time=start_time, 
            end_time=end_time,Ca_start=Ca[RESULT_DATETIME == min(start_time)],
            Ca_end=Ca[RESULT_DATETIME == max(end_time)],
            Ca_avg=Ca_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(Ca_dif=Ca_end-Ca_start) %>% # change in micromole per L
  mutate(Ca_slope=Ca_dif/duration) %>% # micromole per L per year 
  mutate(Ca_rc=(Ca_dif/Ca_start)*100) %>% # percentage of OG value
  distinct() %>%
  # removing stations that only recorded one CO3 value ( ~1000 stations )
  filter(duration!=0) %>%
  merge(locs, by="STAT_ID") %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude))

####### Sodium ####### 
sampling_na <- df %>%
  group_by(STAT_ID) %>%
  filter(!is.na(Na)) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, Na=Na, Na_avg = mean(Na, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  # get daily averages for locations that have multiple per day
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, Na_avg) %>%
  summarize(Na = mean(Na)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(start_time=start_time, 
            end_time=end_time,Na_start=Na[RESULT_DATETIME == min(start_time)],
            Na_end=Na[RESULT_DATETIME == max(end_time)],
            Na_avg=Na_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(Na_dif=Na_end-Na_start) %>% # change in micromole per L
  mutate(Na_slope=Na_dif/duration) %>% # micromole per L per year 
  mutate(Na_rc=(Na_dif/Na_start)*100) %>% # percentage of OG value
  distinct() %>%
  # removing stations that only recorded one CO3 value ( ~1000 stations )
  filter(duration!=0) %>%
  merge(locs, by="STAT_ID") %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude))

####### Potassium ####### 
sampling_k <- df %>%
  group_by(STAT_ID) %>%
  filter(!is.na(Ca)) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, K=K, K_avg = mean(K, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  # get daily averages for locations that have multiple per day
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, K_avg) %>%
  summarize(K = mean(K)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(start_time=start_time, 
            end_time=end_time,K_start=K[RESULT_DATETIME == min(start_time)],
            K_end=K[RESULT_DATETIME == max(end_time)],
            K_avg=K_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(K_dif=K_end-K_start) %>% # change in micromole per L
  mutate(K_slope=K_dif/duration) %>% # micromole per L per year 
  mutate(K_rc=(K_dif/K_start)*100) %>% # percentage of OG value
  distinct() %>%
  # removing stations that only recorded one CO3 value ( ~1000 stations )
  filter(duration!=0) %>%
  merge(locs, by="STAT_ID") %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude))

########################

# Save all dataframes into csvs to import into QGIS
write.csv(sampling_alkalinity, "samples_alkalinity.csv")
write.csv(sampling_ca, "samples_calcium.csv")
write.csv(sampling_k, "samples_potassium.csv")
write.csv(sampling_mg, "samples_magnesium.csv")
write.csv(sampling_mg, "samples_sodium.csv")

######## Clean Data ########
# Making prettier dataframe in order to make more intelligible plots
# Plot-making functions are in plot_functions.R
alk <- sampling_alkalinity %>%
  # Round duration value (in years, rounded to the 0.1)
  mutate(duration=round(duration, 1)) %>%
  # Merge catchment property data in order to make lithology plots
  merge(catchment, by="STAT_ID", all.x=TRUE) %>%
  # Reorder columns 
  select(STAT_ID, duration, sc, sm, Shape_Area, everything()) %>%
  # Only need columns 1-11
  select(1:11) %>%
  # Create classes of slope change
  mutate(slope=ifelse(Alk_slope>0, "augmented", "diminished")) %>%
  mutate(slope=ifelse(Alkalinity_rc<1 & Alkalinity_rc>-1, "stayed the same", slope)) %>%
  # Obtain annual runoff and precipitation data from sampling locations
  merge(runoff, by="STAT_ID", all.x=TRUE) %>%
  merge(rain, by="STAT_ID", all.x=TRUE) %>%
  # Improve column names 
  mutate(Average_Alkalinity=Alkalinity_avg,
         Difference_Alkalinity=Alk_dif,
         Alkalinity_Slope=Alk_slope,
         Relative_Change_Alkalinity=Alkalinity_rc,
         Carbonate_Rock_Abundance=sc,
         Sedimentary_Rock_Abundance=sm,
         Annual_Runoff=annual_runoff,
         Annual_Precipitation=annual_precip,
         Slope_of_Alkalinity=slope) %>%
  # Remove old columns by selecting for only the column indexes we want
  select(1, 2, 5, 15:23)
# This csv will be used to make alkalinity plots in plot_functions.R
write.csv(alk, "alkalinity_data.csv")

########  Data Preparation for RShiny #############

# get region for each ID 
# source: https://github.com/dbouquin/IS_608/blob/master/NanosatDB_munging/Countries-Continents.csv
continents <- read.csv("Countries-Continents.csv")
regions <- locs %>%
  select(STAT_ID, Country, Latitude, Longitude) %>%
  # recode column values to match continents dataframe
  mutate(Country=recode(Country, "USA"="US",
                        "South Corea"="South Korea",
                        "Kenia"="Kenya",
                        "Sengal"="Senegal",
                        "TANZANIA, The United Republic of"="Tanzania")) %>%
  merge(continents, by='Country', all.x=TRUE) %>%
  # Upon further digging I found that the STAT_IDs also reveal the region given the ranges below
  mutate(Continent=ifelse(is.na(Continent)&STAT_ID>99999&STAT_ID<199999, 
                          "North America", Continent)) %>%
  mutate(Continent=ifelse(is.na(Continent)&STAT_ID>199999&STAT_ID<299999, 
                          "South America", Continent))%>% 
  mutate(Continent=ifelse(is.na(Continent)&STAT_ID>299999&STAT_ID<399999, 
                          "Europe", Continent)) %>%
  mutate(Continent=ifelse(is.na(Continent)&STAT_ID>399999&STAT_ID<499999, 
                          "Africa", Continent)) %>%
  mutate(Continent=ifelse(is.na(Continent)&STAT_ID>499999&STAT_ID<599999, 
                          "Asia", Continent)) %>%
  mutate(Continent=ifelse(is.na(Continent)&STAT_ID>599999&STAT_ID<699999, 
                          "Oceania", Continent)) 

# df_rshiny only includes data that has both hydrochemistry data and catchment
# not all catchments have hydrochemistry data
df_rshiny <- df %>%
  merge(regions, by="STAT_ID", all.x=TRUE) %>%
  mutate(duration=round(duration, 1)) %>%
  merge(catchment, by="STAT_ID") %>%
  select(STAT_ID, duration, Continent, HCO3_avg, HCO3_dif, HCO3_slope, HCO3_rc, sc, sm, Shape_Area) 

# We will use the csv in the app.R which creates the RShiny
write.csv(df_rshiny, "data_clean.csv")



