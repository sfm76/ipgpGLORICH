

# Packages
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load in data cleaned
data_clean <- read.csv("data_clean.csv")
data_duration <- read.csv("data_duration.csv")
data_classes <- read.csv("data_classes.csv") 
alk <- read.csv("alkalinity_data.csv")
# Extracted lithology data (carbonate rock abundance) from GLORICH's catchment_properties.csv
lith2 <- read.csv("lithology.csv")

# Make column names more readable
carb_refined <- data_classes %>%
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
  select(2, 3, 20:34)

# Provide source of helper functions for plotting
source("plot_functions.R")

# Define UI for application
ui <- navbarPage(
    "Explore data by:",
    tabPanel("General metadata", 
             navbarPage("",
                        tabPanel("Duration Distribution",
                                 tabsetPanel(tabPanel("All",
                                                      plotOutput(outputId="durplot1")),
                                            tabPanel("By region",
                                                      plotOutput(outputId="durplot3"),
                                                      plotOutput(outputId="durplot4"),
                                                      plotOutput(outputId="durplot2"))
                                            )),
                        tabPanel("Regional Distribution",
                                 plotOutput(outputId="regionplot2"),
                                 wellPanel(fluidRow(column(radioButtons(inputId="class4", "Classify by:", 
                                                       choices=c("Catchment_Size","Carbonate_classification",
                                                                 "Sedimentary_classification","Slope_of_HCO3","Continent",
                                                                 "Runoff", "Precipitation")), width=4),
                                                    column(br(), actionButton("button4", "Plot"), width=2))),
                                 plotOutput(outputId="regionplot1")),
                        tabPanel("All distributions/histograms",
                                 selectInput(inputId="hist1", label="Choose a variable to see the distribution of: ",
                                              choices=c("Average_HCO3", "Difference_HCO3", "Slope_HCO3",
                                                        "Relative_Change_HCO3", "Carbonate_Rock_Abundance",
                                                        "Sedimentary_Rock_Abundance", "Catchment_Area_m2", 
                                                        "Annual_Runoff", "Annual_Precipitation")),
                                 plotOutput(outputId="all"))
                        )),
    tabPanel("Bicarbonate Data",
             tabsetPanel(tabPanel("Parametized", 
                                  wellPanel(fluidRow(
                                    column(selectInput(inputId="type1", "Parameter", 
                                                       choices=c("Average_HCO3","Relative_Change_HCO3")), width=4),
                                    column(radioButtons(inputId="class1", "Classify by:", 
                                                        choices=c("Carbonate_classification",
                                                                  "Sedimentary_classification","Slope_of_HCO3", "Continent",
                                                                  "Precipitation")), width=4),
                                    column(br(), actionButton("button1", "Plot"), width=2)),
                                    textInput(inputId = "split1", label="Choose cutoff between low and high rock abundance (for carbonate and sedimentary classifications):")),
                                  plotOutput(outputId="bicarbplot1")),
                         tabPanel("Over Time", plotOutput(outputId = "bicarbplot2"))
                         )),
    tabPanel("Lithology Data",
             navbarPage("",
                        tabPanel("Distribution",
                                 plotOutput(outputId="lithplot1")),
                        tabPanel("Carbonate / Sedimentary", 
                            textInput(inputId = "split2", label="Choose cutoff between low and high rock abundance (for carbonate and sedimentary classifications):"),
                              tabsetPanel(tabPanel("General",
                                                      fluidRow(column(plotOutput(outputId="lithplot2"), width=6),
                                                                        column(plotOutput(outputId="lithplot3"), width=6))),
                                          tabPanel("Classified (high/low carb)",
                                                               plotOutput(outputId="lithplot6"),
                                                               plotOutput(outputId="lithplot7")),
                                          tabPanel("Compounded with climate",
                                                               plotOutput(outputId="climateplot4")),
                                           tabPanel("By region",
                                                               plotOutput(outputId="lithplot4"),
                                                               plotOutput(outputId="lithplot5"))))
                        )),
    tabPanel("Climate Data", 
                           wellPanel(fluidRow(
                           column(selectInput(inputId="type2", "Parameter", 
                                                       choices=c("Runoff","Precipitation")), width =4),
                           column(radioButtons(inputId="class2", "Classify by:", 
                                                        choices=c("Carbonate_classification", "Sedimentary_classification")), width=4)),
                           textInput(inputId = "split3", label="Choose cutoff between low and high rock abundance (for carbonate and sedimentary classifications):"),
                           actionButton("button2", "Plot")),
                           plotOutput(outputId="climateplot1"),
                           plotOutput(outputId="climateplot2")),
    # Unsure if this alkalinity tab works on the public rshiny link
    tabPanel("Alkalinity", 
             selectInput(inputId="histogram", label="Choose a variable to see the distribution of: ",
                         choices=c("Average_Alkalinity", "Difference_Alkalinity", "Slope_Alkalinity",
                                   "Relative_Change_Alkalinity", "Carbonate_Rock_Abundance",
                                   "Sedimentary_Rock_Abundance", 
                                   "Annual_Runoff", "Annual_Precipitation")),
             plotOutput(outputId = "histograms"),
             plotOutput(outputId = "duration"),
             wellPanel(fluidRow(
               column(selectInput(inputId="boxtype", "Parameter", 
                                  choices=c("Average_Alkalinity","Relative_Change_Alkalinity")), width=4),
               column(radioButtons(inputId="boxclass", "Classify by:", 
                                   choices=c("Carbonate_classification",
                                             "Sedimentary_classification",
                                             "Annual_Precipitation")), width=4),
               column(br(), actionButton("boxbutton", "Plot"), width=2)),
               textInput(inputId = "boxsplit", label="Choose cutoff between low and high rock abundance (for carbonate and sedimentary classifications):")),
             plotOutput(outputId = "boxplot"),
             plotOutput(outputId = "abundance"),
             textInput(inputId = "lithsplit", label="Choose cutoff between low and high rock abundance (for carbonate and sedimentary classifications):"),
             plotOutput(outputId = "lith1"),
             plotOutput(outputId = "lith2"),
             wellPanel(fluidRow(
               column(selectInput(inputId="climatetype", "Parameter", 
                                  choices=c("Runoff","Precipitation")), width =4),
               column(radioButtons(inputId="climateclass", "Classify by:", 
                                   choices=c("Carbonate_classification", "Sedimentary_classification")), width=4)),
               textInput(inputId = "climatesplit", label="Choose cutoff between low and high rock abundance (for carbonate and sedimentary classifications):"),
               actionButton("climatebutton", "Plot")),
             plotOutput(outputId = "climate")
             )
)

# Define server logic 
server <- function(input, output) {
  
  # Outputs that use functions from plot_functions.R have plots that depend on the user input
  #   where the parameters of the function equate ot the user's input
  
  output$all <- renderPlot({
    plot_hist(input$hist1)
  })
  
  output$durplot1 <- renderPlot({
    ggplot(data_clean, aes(x = duration)) +
      geom_histogram(fill = "yellow", color = "black", bins = 20) +
      labs(x = "Duration", y = "Frequency") +
      ggtitle("Duration of GLORICH Data Collection") 
})
  
  output$durplot2 <- renderPlot({
    ggplot(data_clean, aes(x = duration, fill=Continent)) +
      # Facet wrapping will give you a plot for each Continent 
      facet_wrap(~Continent, nrow=2, scales="free") +
      geom_histogram(color = "black", bins = 20, lwd=0.2) +
      labs(x = "Duration", y = "Frequency") +
      ggtitle("Distribution of GLORICH Data Collection Duration") +
      scale_x_continuous(limits=c(0,45)) + 
      scale_y_continuous(limits=c(0,85)) +
      theme_minimal() + guides(fill="none") # removes legend
  })
  
  output$durplot3 <- renderPlot({
    ggplot(data_clean, aes(x = duration, fill = Continent)) + # color based on Continent
      geom_histogram(color="black", lwd=0.2, bins =20) +
      labs(x = "Duration", y = "STAT_ID Frequency", color = "Continent") +
      ggtitle("Duration Distribution of GLORICH Data Collection") +
      theme1 + theme(legend.position=c(0.8,0.8),
                     legend.key.size=unit(0.6, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  })
  
  output$durplot4 <- renderPlot({
    ggplot(data_duration, aes(x = duration, y=count, fill = region)) +
      # Stacked bar graph based on count of each duration
      geom_bar(position="stack", stat="identity", color="black", lwd=0.2) +
      labs(x = "Duration", y = "STAT_ID Frequency", color = "Continent") +
      ggtitle("Distribution of GLORICH Data Collection Duration") +
      theme(legend.position=c(0.8,0.8),
            legend.key.size=unit(0.6, "cm"),
            legend.background = element_rect(fill=alpha('blue', 0))) 
  })
  
  output$regionplot2 <- renderPlot({
    ggplot(carb_refined, aes(x = Continent, fill = Continent)) +
      # Bar graph, ggplot will calculate the count for each Continent
      geom_bar(stat="count", color="black", lwd=0.2) +
      labs(x = "", y = "STAT_ID Frequency") +
      ggtitle("Stations Per Continent") + guides(fill="none")
    
  })
  
  # Whenever re5() is called the server will wait for an event to occur on the first 
  #   argument (input id = button4), and once the event occurs will run the second argument
  #   (plot_regional() which takes input id class4 as an input)
  re5 <- eventReactive(input$button4,
                       {plot_regional(input$class4)})
  output$regionplot1 <- renderPlot({
    re5() 
  })
  
  re1 <- eventReactive(input$button1, 
                       {plot_bicarbonate(input$type1, input$class1, input$split1)})
  output$bicarbplot1 <- renderPlot({
    re1()
  })
  
  output$lithplot1 <- renderPlot({
    ggplot(lith2, aes(x=makeup)) + 
      # Bar graph, ggplot will calculate the count for each lithological makeup
      # Find legend in GLORICH folder: "Documentation_Final_2019_05_24.pdf"
      geom_bar(stat="count", fill="purple", color="black", lwd=0.2) +
      labs(x="Lithological Characterization", y = "Count") +
      ggtitle("Distribution of Characterizations")
  })
  
  output$lithplot2 <- renderPlot({
    ggplot(data_classes, aes(x=sc, y=HCO3_avg)) +
      # Scatter plot 
      geom_point(size=0.7, color="darkorange") +
      labs(x="Abundance of Carbonate Rocks", y = "Average Bicarbonate")
  })
  
  output$lithplot3 <- renderPlot({
    ggplot(data_classes, aes(x=sm, y=HCO3_avg)) +
      # Scatter plot 
      geom_point(size=0.7, color="purple") +
      labs(x="Abundance of Sedimentary Rocks", y = "Average Bicarbonate")
  })
  
  output$lithplot4 <- renderPlot({
    # HCO3 Average vs Carbonate Conc colored by continent
    ggplot(data_clean, aes(x=sc, y=HCO3_avg, color=Continent)) +
      geom_point(size=1) +
      # Provides line of best fit
      geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.3, color="lightgrey") + 
      labs(x="Abundance of Carbonate Rocks") +
      theme_minimal()
  })
  
  output$lithplot5 <- renderPlot({
    # HCO3 Average vs Carbonate Conc faceted by continent
    ggplot(data_clean, aes(x=sc, y=HCO3_avg, color=Continent )) +
      # Facet wrapping will give one plot for each Continent
      facet_wrap(~Continent, nrow=2, scales="free") +
      geom_point(size=1, show.legend=FALSE) +
      labs(x="Abundance of Carbonate Rocks") +
      geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.1, color="lightgrey") + 
      theme1 # defined above
  })
  output$lithplot6 <- renderPlot({
    plot_lith6(input$split2)
  })  
  output$lithplot7 <- renderPlot({
    plot_lith7(input$split2)
  })  
  
  re2 <- eventReactive(input$button2, 
                       {plot_climate1(input$class2, input$split3)})
  output$climateplot1 <- renderPlot({
    re2()
  })

  re3 <- eventReactive(input$button2, 
                       {plot_climate2(input$type2, input$class2, input$split3)})
  output$climateplot2 <- renderPlot({
    re3()
  })
  
  re4 <- eventReactive(input$button3, 
                       {plot_climate_month(input$type3, input$class3)})
  output$climateplot3 <- renderPlot({
    re4()
  })
  
  output$climateplot4 <- renderPlot({
    plot_climate4(input$split2)
  })
  
  output$histograms <- renderPlot({
    plot_hist_alk(input$histogram)
  })
  
  output$duration <- renderPlot({
    ggplot(alk, aes(x = duration)) +
      geom_histogram(fill = "yellow", color = "black", bins = 30) +
      labs(x = "Duration", y = "Frequency") +
      ggtitle("Duration of GLORICH Data Collection") 
  })
  
  reBox <- eventReactive(input$boxbutton, 
                         {plot_alkalinity(input$boxtype, input$boxclass, input$boxsplit)})
  
  output$boxplot <- renderPlot({
    reBox()
  })
  
  output$abundance <- renderPlot({
    ggplot(alk, aes(x=Carbonate_Rock_Abundance, y=Average_Alkalinity)) +
      geom_point(size=0.7, color="darkorange") +
      labs(x="Abundance of Carbonate Rocks", y = "Average Alkalinity")
  })
  
  output$lith1 <- renderPlot({
    plot_alkalinity_lith(input$lithsplit)
  })
  
  output$lith2 <- renderPlot({
    plot_alkalinity_lith2(input$lithsplit)
  })
  
  reClimate <- eventReactive(input$climatebutton, 
                             plot_climate2_alk(input$climatetype, input$climateclass, input$climatesplit))
  
  output$climate <- renderPlot({
    reClimate()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

