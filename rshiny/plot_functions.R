# Packages
library(tidyverse)
library(dbplyr)
library(ggplot2)

# Load in data
data_clean <- read.csv("data_clean.csv")
data_duration <- read.csv("data_duration.csv")
carb2 <- read.csv("data_classes.csv") 
monthly_rain <- read.csv("precipitation.csv")
monthly_runoff <- read.csv("runoff.csv")
lith2 <- read.csv("lithology.csv")
alk <- read.csv("alkalinity_data.csv")

# Theme customizations
theme1 <- theme(axis.text=element_text(size=9),
                axis.title=element_text(size=10),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=8),
                plot.title=element_text(size=10),
                plot.tag=element_text(size=8),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Make readable columns
carb_refined <- carb2 %>%
  mutate(Average_HCO3=HCO3_avg,
         Difference_HCO3=HCO3_dif,
         Slope_HCO3=HCO3_slope,
         Relative_Change_HCO3=HCO3_rc,
         Carbonate_Rock_Abundance=sc,
         Sedimentary_Rock_Abundance=sm,
         Catchment_Area_m2=Shape_Area,
         Annual_Runoff=annual_runoff,
         Annual_Precipitation=annual_precip,
         Catchment_Size=size,
         Carbonate_classification=carbonate,
         Sedimentary_classification=sedimentary,
         Slope_of_HCO3=slope,
         Runoff=runoff,
         Precipitation=precip) %>%
  # Select columns we want by index
  select(2, 3, 20:34)

######## General Data ###########

# Makes histogram for whatever column name user inputted as var
# Destination on RShiny app: General metadata -> All distributions/histograms 
plot_hist <- function(var) {
  ggplot(carb_refined, aes(x=.data[[var]])) +
    geom_histogram(fill="lightpink", color="black", bins = 20, lwd=0.2) 
} 

# Bar graph with count for each Continent, colored based on class selected by user
# Destination on RShiny app: General metadata -> Regional Distribution
plot_regional <- function(class) {
  g <- ggplot(carb_refined, aes(x = Continent, fill = .data[[class]])) +
    geom_bar(stat="count", color="black", lwd=0.2) +
    labs(x = "", y = "STAT_ID Frequency") +
    ggtitle("Stations Per Continent") 
  return(g)
  
  # Functions that load the plot into a variable and then return it like this one are reactive events
}

###############  Bicarbonate Parameters Boxplot w Classifications ##################

# Distibution of bicarbonate average / relative change based on lithology
# Destination on RShiny app: Bicarbonate Data -> Parametized
plot_bicarbonate <- function(type, classification, conc) {
  # Create high vs low classification based on value in conc chosen by user
  df <- carb_refined %>%
    select(1:11, 15, 17) %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>conc, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>conc, "high abundance", "low abundance"))
  # Provide different output based on what user selected for plot type
  if ( type == "Relative_Change_HCO3" ) {
    # Boxplot where boxes depend on classification selected by user (carbonate or sedimentary)
    g = ggplot(df, aes(y=.data[[classification]], x=.data[[type]])) +
      geom_boxplot(fill="lightblue", color="grey30", lwd=0.2, outlier.size = 0.5) +
      scale_x_continuous(limits=c(0,750)) +
      theme_light()
  }
  if ( type == "Average_HCO3") {
    g = ggplot(df, aes(y=.data[[classification]], x=.data[[type]])) +
      geom_boxplot(fill="lightblue", color="grey30", lwd=0.2, outlier.size = 0.5) +
      theme_light()
  }
  return(g)
}

############## Lithological Characteristics ############

# Distributions of bicarbonate averages for each carbonate classification (high / low)
# Destination on RShiny app: Lithology Data -> Carbonate / Sedimentary -> Classified (high/low carb)
plot_lith6 <- function(split) {
  # Create high vs low classification based on value in conc chosen by user
  df <- carb_refined %>%
    select(1:11, 15, 17) %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  ggplot(df, aes(x = Average_HCO3, fill=Carbonate_classification)) +
    # Facet wrapping plots a plot for each classification of carbonate (high and low)
    facet_wrap(~Carbonate_classification, nrow=2) +
    geom_histogram(color = "black", bins = 20, lwd=0.2) +
    labs(x = "Average HCO3", y = "Frequency") +
    ggtitle("Distribution of Bicarbonate Averages") +
    theme1 + guides(fill="none")
}

# Scatter plot colored by carbonate classification
# Destination on RShiny app: Lithology Data -> Carbonate / Sedimentary -> Classified (high/low carb)
plot_lith7 <- function(split) {
  # Create high vs low classification based on value in conc chosen by user
  df <- carb_refined %>%
    select(1:11, 15, 17) %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  ggplot(df, aes(x=Average_HCO3, y=Relative_Change_HCO3, 
                           color=Carbonate_classification, fill=Carbonate_classification)) +
    geom_point(size=0.8) +
    scale_y_continuous(limits=c(0,1000)) +
    scale_x_continuous(limits=c(0,10000)) +
    labs (y="Relative Change")
}

########## Climate #####################

# Scatterplots of Relative change vs. mean for each type of precipitation and carbonate abundance
# Destination on RShiny app: Lithology Data -> Carbonate / Sedimentary -> Compounded with climate
plot_climate4 <- function(split) {
  # Create high vs low classification based on value in conc chosen by user
  df <- carb_refined %>%
    select(1:12, 15, 17) %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  # Facetwraps (separates into different plots for different values of variable) based on two variables 
  ggplot(df, aes(x=Relative_Change_HCO3, y=Average_HCO3)) +
    facet_wrap(~Precipitation+Carbonate_classification, nrow=3) +
    scale_x_continuous(limits=c(-100,400)) +
    geom_point(size=0.2, color="deeppink1") 
}

# Scatterplot for sanity check that precipitation and runoff are correlated, distinguishing lithologies
# Destination on RShiny app: Climate Data -> first plot
plot_climate1 <- function(class, split) {
  # Create high vs low classification based on value in conc chosen by user
  df <- carb_refined %>%
    select(1:12, 15, 17) %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  g <- ggplot(df, aes(x=Annual_Precipitation, y=Annual_Runoff, color=.data[[class]])) +
      geom_point(size=0.3) + 
    theme(legend.position=c(0.8,0.8),
                                   legend.key.size=unit(0.6, "cm"),
                                   legend.background = element_rect(fill=alpha('blue', 0))) 
  return(g)
}

# Scatter plot showing Average Bicarbonate versus climate parameters distinguishing between lithology types 
# Destination on RShiny app: Climate Data -> second plot
plot_climate2 <- function(parameter, class, split) {
  # Create high vs low classification based on value in conc chosen by user
  df <- carb_refined %>%
    select(1:12, 15, 17) %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  # Provide different output based on value user inputted for parameter
  if(parameter=="Runoff") {
    g=ggplot(df, aes(x=Average_HCO3, y=Annual_Runoff, color=.data[[class]])) +
      labs(y="Runoff (mm)") +
      geom_point(size=0.1) + theme(legend.position=c(0.8,0.8),
                                   legend.key.size=unit(0.6, "cm"),
                                   legend.background = element_rect(fill=alpha('blue', 0))) 
  }
  if(parameter=="Precipitation") {
    g=ggplot(df, aes(x=Average_HCO3, y=Annual_Precipitation, color=.data[[class]])) +
      labs(y="Precipitation (mm)") +
      geom_point(size=0.1) + theme(legend.position=c(0.8,0.8),
                                   legend.key.size=unit(0.6, "cm"),
                                   legend.background = element_rect(fill=alpha('blue', 0))) 
  }
  return(g)
}

########### Alkalinity Plots ##############

# Using same plots as above but using alkalinity instead of HCO3

plot_hist_alk <- function(var) {
  
  g <- ggplot(alk, aes(x=.data[[var]])) +
    geom_histogram(fill="lightpink", color="black", bins = 20, lwd=0.2) 
  
  return(g)
}

plot_alkalinity <- function(type, classification, conc) {
  
  df <- alk %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>conc, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>conc, "high abundance", "low abundance"))
  
  if ( type == "Relative_Change_Alkalinity" ) {
    g = ggplot(df, aes(y=.data[[classification]], x=.data[[type]])) +
      geom_boxplot(fill="lightblue", color="grey30", lwd=0.2, outlier.size = 0.5) +
      scale_x_continuous(limits=c(0,750)) +
      theme_light()
  }
  
  if ( type == "Average_Alkalinity") {
    g = ggplot(df, aes(y=.data[[classification]], x=.data[[type]])) +
      geom_boxplot(fill="lightblue", color="grey30", lwd=0.2, outlier.size = 0.5, 
                   outlier.shape = NA) +
      coord_cartesian(xlim=c(0, 10000))
    theme_light()
  }
  
  return(g)
}

plot_climate2_alk <- function(parameter, class, split) {
  df <- alk %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  if(parameter=="Runoff") {
    g=ggplot(df, aes(x=Average_Alkalinity, y=Annual_Runoff, color=.data[[class]])) +
      labs(y="Runoff (mm)") +
      scale_x_continuous(limits=c(0,15000)) +
      
      geom_point(size=0.1) + theme(legend.position=c(0.8,0.8),
                                   legend.key.size=unit(0.6, "cm"),
                                   legend.background = element_rect(fill=alpha('blue', 0))) 
  }
  if(parameter=="Precipitation") {
    g=ggplot(df, aes(x=Average_Alkalinity, y=Annual_Precipitation, color=.data[[class]])) +
      labs(y="Precipitation (mm)") +
      scale_x_continuous(limits=c(0,15000)) +
      geom_point(size=0.1) + theme(legend.position=c(0.8,0.8),
                                   legend.key.size=unit(0.6, "cm"),
                                   legend.background = element_rect(fill=alpha('blue', 0)))  
  }
  return(g)
}


plot_alkalinity_lith <- function(split) {
  
  df <- alk %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance"))
  
  ggplot(df, aes(x = Average_Alkalinity, fill=Carbonate_classification)) +
    facet_wrap(~Carbonate_classification, nrow=1) +
    geom_histogram(color = "black", bins = 20, lwd=0.2) +
    labs(x = "Average Alkalinity", y = "Frequency") +
    ggtitle("Distribution of Alkalinity Averages") +
    theme1 + guides(fill="none")
}

plot_alkalinity_lith2 <- function(split) {
  
  df <- alk %>%
    mutate(Carbonate_classification=ifelse(Carbonate_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    mutate(Sedimentary_classification=ifelse(Sedimentary_Rock_Abundance>split, "high abundance", "low abundance")) %>%
    filter(!is.na(Carbonate_classification))
  
  ggplot(df, aes(x=Average_Alkalinity, y=Relative_Change_Alkalinity, 
                 color=Carbonate_classification, fill=Carbonate_classification)) +
    geom_point(size=0.8) +
    # geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.4, color="black", size=0.4 ) +
    scale_y_continuous(limits=c(0,1000)) +
    scale_x_continuous(limits=c(0,10000)) +
    labs (y="Relative Change")
}


