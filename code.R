# Packages
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(lubridate)

# Importing data from GLORICH Database
hydro_data <- read.csv("df_hydro.csv")
locs <- read.csv("sampling_locations.csv")
catchment <- read.csv("catchment_properties.csv")

data <- hydro_data %>%
  mutate(RESULT_DATETIME=as.Date(RESULT_DATETIME, format='%d/%m/%Y %H:%M:%S')) %>%
  group_by(STAT_ID) %>%
  summarize(start_time=min(RESULT_DATETIME, na.rm = TRUE),
         end_time=max(RESULT_DATETIME, na.rm = TRUE), STAT_ID=STAT_ID, RESULT_DATETIME=RESULT_DATETIME,
         HCO3=HCO3) %>%
  ungroup() %>%
  select(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3) %>%
  mutate(duration=abs(difftime(start_time, end_time, units="weeks"))/52) %>%
  mutate(duration=gsub(" weeks", "", duration)) %>%
  select(STAT_ID, RESULT_DATETIME, start_time, end_time, duration, HCO3) %>%
  merge(catchment, by="STAT_ID") %>%
  select(STAT_ID, RESULT_DATETIME, start_time, end_time, duration, HCO3, GLC_Artificial, 
         GLC_Managed, GLC_Forest, GLC_Shrubs, GLC_Herbaceous, GLC_Sparce_Veg, GLC_Aquatic_Veg,
         GLC_Water, GLC_Snow_Ice, GLC_bare, GLC_Forest_broad, GLC_Forest_mixed, GLC_Forest_needle,
         GLC_PERC_COV, Popdens_90, Popdens_95, Popdens_00, Modis_NPP_gC.m2a, ET)

##########################

# dimensions of changes in hco3

##########################

df2 <- data %>%
  select(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3) %>%
  filter(!is.na(HCO3)) %>%
  group_by(STAT_ID) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, HCO3=HCO3, HCO3_avg = mean(HCO3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3_avg) %>%
  summarize(HCO3 = mean(HCO3)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(HCO3_start=HCO3[RESULT_DATETIME == min(start_time)],
            HCO3_end=HCO3[RESULT_DATETIME == max(end_time)],
            HCO3_avg=HCO3_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(HCO3_dif=HCO3_end-HCO3_start) %>% # change in micromole per L
  mutate(HCO3_slope=HCO3_dif/duration) %>% # micromole per L per year 
  mutate(HCO3_rc=(HCO3_dif/HCO3_start)*100) %>%# percentage of OG value
  distinct() %>%
  # removing stations that only recorded one HCO3 value ( ~1000 stations )
  filter(duration!=0)


#######################

data2 <- data %>%
  merge(df2, by="STAT_ID") %>%
  merge(locs, by="STAT_ID") %>%
  select(STAT_ID, Latitude, Longitude, HCO3_dif, GLC_Artificial, 
         GLC_Managed, GLC_Forest, GLC_Shrubs, GLC_Herbaceous, GLC_Sparce_Veg, GLC_Aquatic_Veg,
         GLC_Water, GLC_Snow_Ice, GLC_bare, GLC_Forest_broad, GLC_Forest_mixed, GLC_Forest_needle,
         GLC_PERC_COV, Popdens_90, Popdens_95, Popdens_00, Modis_NPP_gC.m2a, ET) %>%
  distinct() %>%
  filter(!is.na(HCO3_dif)) %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude),
         ET=as.numeric(ET))
  
#######################

# row per station, includes lat and long, hco3 difference & slope, duration of collection, 
# and catchment data such as GLC, Popdens, NPP, and ET

#######################
# 
data3 <- data %>%
  merge(df2, by="STAT_ID") %>%
  merge(locs, by="STAT_ID") %>%
  select(STAT_ID, Latitude, Longitude, HCO3_dif, duration, HCO3_slope, GLC_Artificial, 
         GLC_Managed, GLC_Forest, GLC_Shrubs, GLC_Herbaceous, GLC_Sparce_Veg, GLC_Aquatic_Veg,
         GLC_Water, GLC_Snow_Ice, GLC_bare, GLC_Forest_broad, GLC_Forest_mixed, GLC_Forest_needle,
         GLC_PERC_COV, Popdens_90, Popdens_95, Popdens_00, Modis_NPP_gC.m2a, ET) %>%
  distinct() %>%
  filter(!is.na(HCO3_dif)) %>%
  mutate(Latitude=as.numeric(Latitude),
         Longitude=as.numeric(Longitude),
         duration=as.numeric(time_dif),
         ET=as.numeric(ET))

######################
# region for each ID 

continents <- read.csv("Countries-Continents.csv")

regions <- locs %>%
  select(STAT_ID, Country, Latitude, Longitude) %>%
  mutate(Country=recode(Country, "USA"="US",
                                  "South Corea"="South Korea",
                                  "Kenia"="Kenya",
                                  "Sengal"="Senegal",
                        "TANZANIA, The United Republic of"="Tanzania")) %>%
  merge(continents, by='Country', all.x=TRUE) %>%
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
######################

# data4 only includes data that is in both hydrochemistry and catchment 
# not all sampling locations have catchment property data
data4 <- df2 %>%
  merge(regions, by="STAT_ID", all.x=TRUE) %>%
  mutate(duration=round(duration, 1)) %>%
  merge(catchment, by="STAT_ID") %>%
  select(STAT_ID, duration, Continent, HCO3_avg, HCO3_dif, HCO3_slope, HCO3_rc, sc, sm, Shape_Area) 

catchment <- catchment %>%
  #merge(regions, by='STAT_ID', all.x=TRUE) 
  mutate(has_HCO3=ifelse(STAT_ID %in% df2$STAT_ID, TRUE, FALSE)) 
  #select(STAT_ID, Continent, has_HCO3, everything()) 

  
data1 <- df2 %>%
  merge(regions, by="STAT_ID", all.x=TRUE) %>%
  mutate(duration=round(duration, 1)) 
data1 <- data1 %>%
  merge(catchment, by="STAT_ID", all.x=TRUE) 
  select(STAT_ID, duration, Continent, HCO3_avg, HCO3_dif, HCO3_slope, HCO3_rc, sc, sm, Shape_Area) 


##########################

data_lith <- data4 %>%
    mutate(carbonate=ifelse(sc>0.5, "high carbonate", "low carbonate"),
           sedimentary=ifelse(sm>0.50, "high sedimentary", "low sedimentary")) %>%
    select(STAT_ID, duration, Continent, HCO3_avg, HCO3_dif, HCO3_slope, HCO3_rc, 
           carbonate, sedimentary) 
  
conc <- data_lith %>%  # classifications of lithology with IDs
  select(STAT_ID, carbonate, sedimentary) 

bicarb <- data %>%
  merge(locs, by="STAT_ID") %>%
  filter(!is.na(HCO3)) %>%
  select(1:6, 26:32)
#bicarb <- bicarb %>%
#  merge(conc, by='STAT_ID', all.x = TRUE) 
# bicarb has data for each stat id over time, along with lith classes

carb <- data4 %>%
  merge(conc, by='STAT_ID') # carb has all the parameters per STAT_ID

carb <- carb %>%
  mutate(slope=ifelse(HCO3_slope>0, "augmented", "diminished")) %>%
  mutate(slope=ifelse(HCO3_rc<1 & HCO3_rc>-1, "stayed the same", slope)) 

carb2 <- carb %>%
  merge(runoff, by="STAT_ID") %>%
  merge(rain, by="STAT_ID")

# Finding cutoffs
cutoff <- mean(data4$Shape_Area) 

carb2 <- carb2 %>%
  mutate(size=ifelse(Shape_Area<cutoff, "small", "large")) %>%
  select(STAT_ID, Continent, annual_runoff, annual_precip, everything()) %>%
  mutate(runoff=ifelse(annual_runoff>363, "high runoff", "low runoff")) %>% # 363 =mean
  mutate(precip=ifelse(annual_precip>1500, "high rainfall", "low rainfall")) 

latlong <- regions %>%
  select(STAT_ID, Latitude, Longitude)
carb2_withlocs <- carb2 %>%
  merge(latlong, by="STAT_ID")
  
slope <- carb %>%
  select(STAT_ID, slope)

lith <- catchment %>%
  gather("litho", "concentration", 6:21) %>%
  select( STAT_ID, litho, concentration) %>%
  filter(!concentration==0) %>%
  merge(regions, by="STAT_ID") %>%
  merge(conc, by="STAT_ID") %>%
  merge(slope, by="STAT_ID")


lith2 <- lith %>%
  group_by(STAT_ID) %>%
  mutate(max=max(concentration)) %>%
  mutate(makeup=ifelse(concentration==max, litho, max)) %>%
  filter(litho==makeup) %>%
  select(STAT_ID, makeup, concentration) %>%
  merge(carb2, by="STAT_ID")


# For QGIS land use 
bicarb2000 <- bicarb %>%
  filter(RESULT_DATETIME > "2000-01-01") %>%
  merge(df_2000, by="STAT_ID") 

bicarb_ann2000 <- bicarb2000 %>%
  mutate(year=as.numeric(format(RESULT_DATETIME, "%Y")))
  
df_2000 <- bicarb2000 %>%
  select(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3) %>%
  filter(!is.na(HCO3)) %>%
  group_by(STAT_ID) %>%
  summarize(RESULT_DATETIME=RESULT_DATETIME, start_time=start_time, 
            end_time=end_time, HCO3=HCO3, HCO3_avg = mean(HCO3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(RESULT_DATETIME == start_time | RESULT_DATETIME == end_time, na.rm = TRUE) %>%
  group_by(STAT_ID, RESULT_DATETIME, start_time, end_time, HCO3_avg) %>%
  summarize(HCO3 = mean(HCO3)) %>%
  ungroup() %>%
  group_by(STAT_ID) %>%
  summarize(HCO3_start=HCO3[RESULT_DATETIME == min(start_time)],
            HCO3_end=HCO3[RESULT_DATETIME == max(end_time)],
            HCO3_avg=HCO3_avg,
            duration=as.numeric(max(end_time)-min(start_time))) %>%
  mutate(duration=duration/365) %>%
  mutate(HCO3_dif=HCO3_end-HCO3_start) %>% # change in micromole per L
  mutate(HCO3_slope=HCO3_dif/duration) %>% # micromole per L per year 
  mutate(HCO3_rc=(HCO3_dif/HCO3_start)*100) %>%
  select(!duration)

n <- filter(bicarb2000, is.na(bicarb2000$Latitude))
##########################

# Write to csv to push to python
write.csv(data4, "data_clean.csv")
write.csv(carb2, "data_classes.csv")
write.csv(dur, "data_duration.csv")
write.csv(carb2_withlocs, "bicarbonate_wcoordinates.csv")
write.csv(bicarb2000, "bicarbonate2000_wcoordinates.csv")


#########################

## Plot data 

quartz()

# Theme customization
theme1 <- theme(axis.text=element_text(size=10),
                axis.title=element_text(size=10),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=8),
                plot.title=element_text(size=18),
                plot.tag=element_text(size=8),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))


######## Distribution of Duration / General Data ###########

ggplot(data4, aes(x = Continent, fill = Continent)) +
  geom_bar(stat="count", color="black", lwd=0.2) +
  labs(x = "", y = "STAT_ID Frequency") +
  ggtitle("Stations Per Continent") + guides(fill="none")

# Distribution of classifications across regions
ggplot(data4, aes(x = Continent, fill = size)) +
  geom_bar(position="stack", stat="count", color="black", lwd=0.2) +
  labs(x = "", y = "STAT_ID Frequency") +
  ggtitle("Stations Per Continent") 
ggplot(carb, aes(x = Continent, fill = carbonate)) +
  geom_bar(position="stack", stat="count", color="black", lwd=0.2) +
  labs(x = "", y = "STAT_ID Frequency") +
  ggtitle("Stations Per Continent") 

# df2 has one row per STAT_ID (that has at least 2 HCO3 measurements)
# df2 describes HCO3 behavior parameters for each STAT_ID
ggplot(df2, aes(x = duration)) +
  geom_histogram(fill = "yellow", color = "black", bins = 20) +
  labs(x = "Duration", y = "Frequency") +
  ggtitle("Duration of GLORICH Data Collection") 
  #theme_minimal()
# Per continent
ggplot(data4, aes(x = duration, fill=Continent)) +
  facet_wrap(~Continent, nrow=2, scales="free") +
  geom_histogram(color = "black", bins = 20, lwd=0.2) +
  labs(x = "Duration", y = "Frequency") +
  ggtitle("Distribution of GLORICH Data Collection Duration") +
  scale_x_continuous(limits=c(0,45)) + 
  scale_y_continuous(limits=c(0,85)) +
  theme_minimal() + guides(fill="none")
# Distribution of Duration under 20 years
ggplot(df2 %>% filter(duration<20), aes(x = duration)) +
  geom_histogram(fill = "yellow", color = "black", bins = 20) +
  labs(x = "Duration", y = "Frequency") +
  scale_x_continuous(breaks=seq(0,20,1), lim = c(0,20)) +
  ggtitle("Duration of GLORICH Data Collection (<20 years)") +
  theme_minimal()
# Distribution of Duration over 20 years
ggplot(df2 %>% filter(duration>20), aes(x = duration)) +
  geom_histogram(fill = "yellow", color = "black", bins = 20) +
  labs(x = "Duration", y = "Frequency") +
  ggtitle("Duration of GLORICH Data Collection (>20 years)") +
  theme_minimal()
# Stacked bar graph: duration distribution split by continent
dur <- data4 %>%
  mutate(duration=round(duration, 0)) %>%
  group_by(duration, Continent) %>%
  summarize(duration= duration, count=length(STAT_ID), region=Continent) %>%
  ungroup() %>%
  distinct() %>% 
  select(duration, count, region)
ggplot(dur, aes(x = duration, y=count, fill = region)) +
  geom_bar(position="stack", stat="identity", color="black", lwd=0.2) +
  labs(x = "STAT_ID Frequency", y = "Duration", color = "Continent") +
  ggtitle("Duration Distribution of GLORICH Data Collection") +
  theme1
ggplot(data4, aes(x = duration, fill = Continent)) +
  geom_histogram(color="black", lwd=0.2, bins =20) +
  labs(x = "STAT_ID Frequency", y = "Duration", color = "Continent") +
  ggtitle("Duration Distribution of GLORICH Data Collection") +
  theme1


###########################################

theme2 <- theme(axis.text=element_text(size=6),
                axis.title=element_text(size=8),
                strip.text=element_text(size=8),
                plot.title=element_text(size=9),
                axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

####### HCO3 Average #############

ggplot(carb, aes(y=Continent, x=HCO3_avg)) +
  facet_wrap(~carbonate, nrow=2) +
  geom_boxplot(fill="lightblue", color="grey30", lwd=0.1, outlier.size = 0.5) +
  labs(y="", x="Average HCO3", title="Bicarbonate Averages Across Continents") +
  theme_light()


##################################



####### HCO3 Over Time #############
#hco3 over time (scatter) per continent
ggplot(bicarb, aes(x = RESULT_DATETIME, y = HCO3, color=Continent)) +
  facet_wrap(~Continent, nrow=2, scales="free") +
  geom_point(size=.3, show.legend = FALSE) +
  labs(x = "Time", y = "HCO3") +
  ggtitle("HCO3 Over Time") +
  #scale_x_continuous(limits=c(1940,2010)) + 
  #scale_y_continuous(limits=c(0,40000)) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.0, color="black", lwd=0.1) + 
  guides(fill="none") + theme1


###########################################
##### doesn't work
# Line graph of movement of HCO3 change colored by region 
data5 <- data %>%
  filter(!is.na(HCO3)) %>%
  merge(regions, by = 'STAT_ID', all.x = TRUE) %>%
  filter(duration>20) %>%
  filter(Continent=='Africa')
ggplot(data5, aes(x = RESULT_DATETIME, y = HCO3, color = Continent)) + 
  geom_line() + 
  labs(x = "Time", y = "Bicarbonate", color="Continent") +
  ggtitle("Change in Bicarbonate Overtime Across Regions") + 
  theme_minimal()
#######

########## Catchment Lithology Distribution ####################

# Carbonate Distribution across catchments
catchment2 <- catchment %>%
  filter(sc!=0) %>%
  filter(sm!=0)

# Distribution of Carbonate Rock Concentration Across Stations
catchment3 <- catchment2 %>%
  mutate(sc=round(sc, 4), sm=round(sc,4)) %>%
  group_by(sc) %>%
  summarize(frequency=length(STAT_ID)) %>%
  filter(sc!=0.0000)
ggplot(catchment3, aes(x=sc, y = frequency)) +
  geom_bar(stat="identity", color="orange", l="purple") +
  labs(x = "Percentage of Carbonate Rock Concentration", y = "Number of Stations") +
  ggtitle("Distribution of Carbonate Rock") +
  theme_light()
ggplot(catchment, aes(x = sc)) +
  geom_histogram(fill="orange", color="purple", bins=20) +
  labs(x="Percentage of Carbonate Rock Concentration", y = "Number of Stations") +
  ggtitle("Distribution of Carbonate Rock") + 
  theme_light()

ggplot(catchment, aes(x = sc)) +
  facet_wrap(~Continent, nrow=3) +
  geom_histogram(fill="orange", color="black", bins=20, lwd=0.2) +
  labs(x="Percentage of Carbonate Rock Concentration", y = "Number of Stations") +
  ggtitle("Distribution of Carbonate Rock") + 
  theme_light()
ggplot(catchment, aes(x = sm)) +
  geom_histogram(fill="purple", color="orange", bins=20) +
  labs(x="Percentage of Sedimentary Rock Concentration", y = "Number of Stations") +
  ggtitle("Distribution of Sedimentary Rock") + 
  theme_light()
ggplot(catchment, aes(x = sm)) +
  facet_wrap(~Continent, nrow=3) +
  geom_histogram(fill="purple", color="black", bins=20, lwd=0.2) +
  labs(x="Percentage of Sedimentary Rock Concentration", y = "Number of Stations") +
  ggtitle("Distribution of Sedimentary Rock") + 
  theme_light()

catchment4 <- catchment %>%
  select(Catch_ID, Shape_Area, sc, sm) %>%
  gather(key="ratio_type", value="ratio", 3:ncol(.)) %>%
  mutate(ratio_type=recode_factor(ratio_type, "sc"="Carbonate rock", "sm"= "Sedimentary rock"))

############### Area ########################

ggplot(data4, aes(x = Shape_Area)) +
  geom_histogram(fill = "yellow", color = "black", bins = 20) +
  labs(x = "Catchment Area (m^2)", y = "Frequency") +
  ggtitle("Distribution of Catchment Area Size") +
  theme_light()
# Per continent
ggplot(data4, aes(x = Shape_Area, fill=Continent)) +
  facet_wrap(~Continent, nrow=2, scales="free") +
  geom_histogram(color = "black", bins = 20, lwd=0.2) +
  labs(x = "Catchment Area (m^2)", y = "Frequency") +
  ggtitle("Distribution of Catchment Area Size") +
  theme_light() + guides(fill="none")


ggplot(data4, aes(x = Shape_Area, fill=size)) +
  facet_wrap(~size, nrow=2, scales="free") +
  geom_histogram(color = "black", bins = 50, lwd=0.2) +
  labs(x = "Catchment Area (m^2)", y = "Frequency") +
  ggtitle("Distribution of Catchment Area Size") +
  theme_light() + guides(fill="none")

# HCO3 Relative Change across different catchment sizes
ggplot(data4, aes(x = HCO3_rc, fill=size)) +
  facet_wrap(~size, nrow=2) +
  geom_histogram(color = "black", bins = 50, lwd=0.2) +
  labs(x = "HCO3", y = "Frequency") +
  ggtitle("Relative Change") +
  theme_light() + guides(fill="none")

#############################################

# Lithology Scatters


# HCO3 Average vs Carbonate Conc colored by continent
ggplot(data4, aes(x=sc, y=HCO3_rc, color=Continent)) +
  geom_point(size=1) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.5, color="lightgrey") + 
  theme_minimal()
# HCO3 Average vs Carbonate Conc faceted by continent
ggplot(data4, aes(x=sc, y=HCO3_avg, color=Continent, )) +
  facet_wrap(~Continent, nrow=2, scales="free") +
  geom_point(size=1, show.legend=FALSE) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.5, color="lightgrey") + 
  theme1 
# HCO3 Slope vs Carbonate Conc faceted by continent
ggplot(data4, aes(x=sc, y=HCO3_slope, color=Continent )) +
  facet_wrap(~Continent, nrow=2, scales="free") +
  geom_point(size=1, show.legend=FALSE) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.5, color="lightgrey") + 
  theme1 
# HCO3 Relative Change vs Carbonate Conc faceted by continent
ggplot(data4, aes(x=sc, y=HCO3_rc, color=Continent )) +
  facet_wrap(~Continent, nrow=2, scales="free") +
  geom_point(size=1, show.legend=FALSE) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.5, color="lightgrey") + 
  theme1 

# HCO3 Relative Change Distribution faceted by carb conc
ggplot(carb, aes(x = HCO3_rc, fill=carbonate)) +
  facet_wrap(~carbonate, nrow=2) +
  geom_histogram(color = "black", bins = 50, lwd=0.2) +
  labs(x = "HCO3 Relative Change", y = "Frequency") +
  scale_x_continuous(limits=c(-100,250)) +
 # scale_y_continuous(limits=c(0,120)) +
  ggtitle("Distribution of Bicarbonate Relative Change") +
  theme1 + guides(fill="none")

# HCO3 Slope Distribution faceted by carb conc
ggplot(carb, aes(x = HCO3_slope, fill=carbonate)) +
  facet_wrap(~carbonate, nrow=2) +
  geom_histogram(color = "black", bins = 20, lwd=0.2) +
  labs(x = "HCO3 Slope", y = "Frequency") +
  ggtitle("Distribution of Bicarbonate Slope") +
  theme1 + guides(fill="none")

# HCO3 Average Distribution faceted by carb conc
ggplot(carb, aes(x = HCO3_avg, fill=carbonate)) +
  facet_wrap(~carbonate, nrow=2) +
  geom_histogram(color = "black", bins = 20, lwd=0.2) +
  labs(x = "HCO3 Mean", y = "Frequency") +
  ggtitle("Distribution of Bicarbonate Relative Change") +
  theme1 + guides(fill="none")

# Carbonate Concentration faceted by slope
ggplot(carb, aes(x=sc, fill=slope)) + 
  facet_wrap(~slope, nrow=3) + 
  geom_histogram(color="black", bins = 20, lwd=0.2) + 
  labs(x = "Carbonate Concentration", y = "Frequency") + 
  ggtitle("Lithology") +
  theme1 + guides(fill="none")
ggplot(carb, aes(x=carbonate, fill=carbonate)) + 
  facet_wrap(~slope, nrow=3) + 
  geom_histogram(stat="count", color="black", bins = 20, lwd=0.2) + 
  labs(x = "Carbonate Concentration", y = "Frequency") + 
  ggtitle("Lithology") +
  theme1 + guides(fill="none")
# stayed the same lithology
ggplot(carb%>%filter(slope=="stayed the same"), aes(x=carbonate, fill=carbonate)) + 
  geom_histogram(stat="count", color="black", bins = 20, lwd=0.2) + 
  labs(x = "Carbonate Concentration", y = "Frequency") + 
  ggtitle("Lithology") +
  theme1 + guides(fill="none")

ggplot(data4, aes(x=sm, y=HCO3_avg, color=Continent)) +
  geom_point(size=1) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.0, color="lightgrey" ) +
  theme_minimal()

ggplot(data4, aes(x=sm, y=sc, color=Continent)) + 
  geom_point(size=1) +
  theme_minimal()

ggplot(data_lith, aes(x=HCO3_avg, y=HCO3_rc, color=carbonate, fill=carbonate)) +
  geom_point(size=0.7) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.4, color="black", size=0.4 ) +
  theme_minimal()

########### Tropics / Temperate ######################
########### Compounding Variables ####################


runoff <- catchment %>%
  select(1, 39:64) %>%
  gather("month", "runoff", 2:13) %>%
  mutate(month=recode(month, "q_01"="January",
                      "q_02"="February", "q_03"="March", "q_04"="April",
                      "q_05"="May", "q_06"="June", "q_07"="July",
                      "q_08"="August", "q_09"="September", "q_10"="October",
                      "q_11"="November", "q_12"="December", "q_ann"="Annual"),
         annual_runoff=q_ann) %>%
  select(STAT_ID, annual_runoff, month, runoff) 
  # delete if you need the month values back
  #select(STAT_ID, annual_runoff) %>%
  #distinct()
  
rain <- catchment %>%
  select(1, 52:64) %>%
  gather("month", "precipitation", 2:13) %>%
  mutate(month=recode(month, "Hijm_P_01"="January",
                      "Hijm_P_02"="February", "Hijm_P_03"="March", "Hijm_P_04"="April",
                      "Hijm_P_05"="May", "Hijm_P_06"="June", "Hijm_P_07"="July",
                      "Hijm_P_08"="August", "Hijm_P_09"="September", "Hijm_P_10"="October",
                      "Hijm_P_11"="November", "Hijm_P_12"="December", "Hijm_P_ann"="Annual"),
         annual_precip=Hijm_P_ann) %>%
  select(STAT_ID, annual_precip, month, precipitation) 
  # delete if you need the month values back
  #select(STAT_ID, annual_precip) %>%
  #distinct()
  

monthly_runoff <- carb %>%
  merge(runoff, by="STAT_ID") 
monthly_rain <- carb %>%
  merge(rain, by="STAT_ID")


pairs(carb2[,3:12], pch = 19, lower.panel=NULL, size=0.05)


ggplot(climate2, aes(x=HCO3_avg, y=precipitation, color=carbonate)) +
  facet_wrap(~month, nrow= 4) +
  geom_point(size=0.2) 
  #geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.4, color="black", size=0.4 ) 

ggplot(carb2, aes(x=annual_precip, y=annual_runoff, color=slope)) +
  #facet_wrap(~Continent, nrow=3) +
  geom_point(size=0.7) + 
  scale_color_manual(values = c("darkgreen", "cadetblue3", "orange")) +
  theme_light()

ggplot(carb2, aes(x=annual_precip)) +
 # facet_wrap(~carbonate, nrow=3) +
  geom_histogram(color="black", bins = 20, lwd=0.2) 

ggplot(carb2, aes(x=annual_runoff, fill=slope)) +
  geom_histogram(color="black", bins=30, lwd=0.2)

ggplot(carb2, aes(x=HCO3_rc)) +
  facet_wrap(~runoff+carbonate, nrow=3) +
  geom_histogram(color="black", fill="purple", bins = 30, lwd=0.2) 

ggplot(carb2, aes(x=annual_precip, y=HCO3_avg)) +
  facet_wrap(~runoff+carbonate, nrow=3) +
  geom_point(size=0.2, color="deeppink1") 

ggplot(carb2, aes(x=HCO3_rc, y=HCO3_avg)) +
  facet_wrap(~runoff+carbonate, nrow=2) +
  scale_x_continuous(limits=c(-100,300)) +
  geom_point(size=0.2, color="darkorange") 
#  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.4, color="black")

ggplot(carb2, aes(x=HCO3_rc, y=HCO3_avg)) +
  facet_wrap(~sedimentary+carbonate, nrow=2) +
  scale_x_continuous(limits=c(-100,300)) +
  geom_point(size=0.2, color="darkorange") 

ggplot(carb2, aes(x=HCO3_rc, y=annual_precip)) +
  facet_wrap(~runoff+carbonate, nrow=3) +
  scale_x_continuous(limits=c(-100,300)) +
  geom_point(size=0.2, color="darkorange") 
#  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.4, color="black")

ggplot(carb2, aes(x=sm, y=HCO3_avg)) +
  geom_point(size=0.5, color="darkorange") 


# no stations with high carbonate + high rainfall 

quartz()
######################################################


####### Load GEMStat data ################
path <- "GEMS-Water_data_request"
gemstat <- read.csv(file.path(path, "Hydrogencarbonate.csv"), sep=";")
stations <- read.csv(file.path(path, "GEMS-Water_data_request.csv"))

gemstat_df <- gemstat %>%
  merge(stations, by="GEMS.Station.Number")

write.csv(gemstat_df, "gemstat_gis.csv")
