#-------------------------------------------------------
# Description
#-------------------------------------------------------

# Author Laura Puckett
# Date 11/26/2021
# Purpose: Join and filter NEON woody vegetation data for visualization 
# in a shiny app. 

#-------------------------------------------------------
# Setup
#-------------------------------------------------------
library(shiny); library(ggplot2); library(dplyr); library(shinythemes)

# load saved datasets
veg = readRDS('./veg_data_for_shiny.Rdata')
species = readRDS('./species.Rdata')

# format input options
input_options = species$num
names(input_options) = species$scientificName

#-------------------------------------------------------
# User Interface
#-------------------------------------------------------

ui = fluidPage(
  theme = shinytheme("readable"),
  h3("NEON Woody Plant Vegetation Structure Data"),
  p("This app allows for quick visualization of tree height and diameter data for the 20 most common species in the NEON woody vegetation structure dataset. The data have been filtered to remove dead trees and obvious outliers for height and diameter. "),
  HTML("<p style='color:#808080'>NEON (National Ecological Observatory Network). Woody plant vegetation structure, RELEASE-2021 (DP1.10098.001). https://doi.org/10.48443/e3qn-xw47. Dataset accessed from https://data.neonscience.org on November 24, 2021 </p>"),
  p("\n"), 
  fluidRow(column(width = 12, selectInput(inputId = "optionNum",
                                          label = "Choose a Species Group",
                                          choices = input_options,
                                          selected = 1))),
  fluidRow(column(width = 1, checkboxInput("Live", "Live", TRUE)),
           column(width = 2, checkboxInput("Live_DD", "Live, disease damaged", TRUE,)),
           column(width = 2, checkboxInput("Live_PD", "Live, physically damaged", TRUE)),
           column(width = 2, checkboxInput("Live_BB", "Live, broken bole", TRUE)),
           column(width = 2, checkboxInput("Live_ID", "Live, insect damaged", TRUE)),
           column(width = 2, checkboxInput("Live_OD", "Live,  other damage", TRUE))),
  
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel(title ="Plot",  plotOutput('plot1')),
                        tabPanel("Data Table",  DT::dataTableOutput('table')))
  )
)
#-------------------------------------------------------
# Server Function
#-------------------------------------------------------

server = function(input, output) {
  selection = reactive({
    as.numeric(input$optionNum)
  })
  selected_data <- reactive({
    df <- veg %>%
      filter(taxonID == species$taxonID[as.numeric(input$optionNum)]) %>%
      dplyr::select(individualID, siteID,scientificName, plantStatus, date, stemDiameter, height)
    
    if(!input$Live){
      df = df %>% filter(plantStatus != "Live")
    }
    if(!input$Live_DD){
      df = df %>% filter(plantStatus != "Live, disease damaged")
    }
    if(!input$Live_PD){
      df = df %>% filter(plantStatus != "Live, physically damaged")
    }
    if(!input$Live_BB){
      df = df %>% filter(plantStatus != "Live, broken bole")
    }
    if(!input$Live_ID){
      df = df %>% filter(plantStatus != "Live, insect damaged")
    }
    if(!input$Live_OD){
      df = df %>% filter(plantStatus != "Live,  other damage")
    }
    
    print(df)
    df
  })
  
  
  output$table <- DT::renderDataTable({
    table <- selected_data()
  })
  
  output$plot1 <- renderPlot({
    ggplot(data = selected_data(),
           aes(x = stemDiameter, y = height)) +
      theme_bw() + 
      geom_point(aes(color = siteID), alpha = 0.4) +
      geom_smooth(color = "black",size = 0.5, se = F) + 
      xlab("Stem Diameter [cm]") + 
      ylab("Tree Height [m]")  +
      # ggtitle(paste("Selected Species:",
      #               species$scientificName[selection()])) +
      labs(colour = "NEON Site ID")+ 
      theme(legend.position = "right",
            axis.title = element_text(size = 16))
  })
  
  output$result <- renderText({
    paste("You have selected ", species$scientificName[selection()])
  })
}

#-------------------------------------------------------
# App call
#-------------------------------------------------------
shinyApp(ui = ui, server = server)

