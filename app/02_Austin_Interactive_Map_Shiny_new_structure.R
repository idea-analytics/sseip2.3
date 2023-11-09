# matching the structure to the Shiny app structure sketch for the enrollment project
# structure ("C:\Users\lindsey.smith\OneDrive - ideapublicschools.org\IDEA\Projects\202310a enrollment - GIS, census\shiny-app-structure.pdf")

# load libraries

library(tidyverse)
library(sf)
library(rdeck)
library(tidycensus)
sf::sf_use_s2(FALSE)
library(shiny)
library(mapboxapi)
library(here)

# set working directory

# setwd("C:/Users/lindsey.smith/OneDrive - ideapublicschools.org/IDEA/Projects/202310a enrollment - GIS, census/proof of concept map/")

## load data

# simple frame school locations
load("sf_schools_austin.RData")
#load(here::here("sf_schools_austin.RData"))
# simple frame student locations
load("sf_students_austin.RData")
# block_group_children
load("pp_children_bg_dots.Rdata")
# drive_times
load("pp_site_drive_15_min_sf.Rdata")

## mapbox token

options(rdeck.mapbox_access_token = Sys.getenv("MAPBOX_API_TOKEN"))

## Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Austin Student Density App + Sample Inputs and Outputs"),
  
  # set up side panel with multiple options
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "order", 
                  label = "Choose an order of mammals",
                  choices = sort(msleep$order)),
      radioButtons(inputId = "type",
                   label = "Choose a type of mammal",
                   choices = sort(unique(msleep$vore))),
      sliderInput(inputId = "brain_wt",
                  label = "Select a brain weight (kg) range",
                  min = min(msleep$brainwt, na.rm = TRUE),
                  max = max(msleep$brainwt, na.rm = TRUE),
                  value = c(quantile(msleep$brainwt, 0.85, na.rm = TRUE), 
                            quantile(msleep$brainwt, 0.99, na.rm = TRUE)),
                  dragRange = TRUE)
    ),
  
  
  # set up tabs
  mainPanel(
    tabsetPanel(
      tabPanel("Map", rdeckOutput("map")),
      tabPanel("Animal Total Sleep", plotOutput("sleep")),
      tabPanel("Body Weight by Brain Weight", plotOutput("weight")),
      tabPanel("Brain Weight", plotOutput("brain_wt")),
      tabPanel("Hello", "Hello World") 
    )
  )
)
)

## Define server logic required to make visuals

server <- function(input, output) {
  
  output$sleep <- renderPlot({
    msleep %>% 
      filter(order == input$order) %>% 
      ggplot(aes(x = name, 
                 y = sleep_total, 
                 fill = name)) +
      geom_col() +
      labs(title = "Distribution of Sleep by Mammal for Order Selected",
           x = "Mammal Name",
           y = "Hours of Sleep per Day")
  })
  
  output$weight <-  renderPlot({
    msleep %>%
      filter(vore == input$type) %>%
      ggplot(aes(x = brainwt, 
                 y = bodywt, 
                 color = name)) +
      geom_point() +
      labs(title = "Body Weight by Brain Weight for Type Selected",
           x = "Brain Weight (kg)",
           y = "Body Weight (kg)")
  })
  
  output$brain_wt <- renderPlot({
    msleep %>%
      filter(between(brainwt, min(input$brain_wt), max(input$brain_wt))) %>%
      ggplot(aes(x = brainwt)) +
      geom_histogram() +
      labs(title = "Distribution of Brain Weight for Range of Brain Weights Selected",
           x = "Brain Weight (kg)",
           y = "Count")
  })

  output$map <- renderRdeck(
    
    rdeck(map_style = mapbox_gallery_frank(), 
          initial_bounds = pp_children_bg_dots, 
          theme = "light", 
          editor = FALSE,
          blending_mode = "subtractive", 
          height = 800
    ) %>% 
      
      add_polygon_layer(data = pp_site_drive_15_min_sf %>% filter(time == 10), 
                        get_polygon = geometry, opacity = .05,
                        get_fill_color = scale_color_category(col=school_short_name, 
                                                              legend = FALSE,
                                                              palette = ideacolors::idea_palettes$qual), 
                        name = "10-min drivetime", 
                        group_name = "Schools",
                        visible = FALSE,
      ) %>% 
      
      add_heatmap_layer(data = sf_students_austin,
                        get_position = geometry,
                        opacity = .25,
                        name = "Current IDEA student density", 
                        group_name = "Heatmaps", 
                        visible = FALSE
      ) %>%
      
      add_heatmap_layer(data = pp_children_bg_dots,
                        get_position = geometry,
                        opacity = .4,
                        visible = FALSE,
                        color_range = viridisLite::turbo(6, direction = 1),
                        name = "Est. school-aged children density", 
                        group_name = "Heatmaps",
                        threshold = .5
      ) %>%
      
      add_scatterplot_layer(data = pp_children_bg_dots ,
                            get_position = geometry,
                            get_fill_color = "#9A95A2",
                            radius_min_pixels = 2,
                            opacity = .25,
                            visible = TRUE,
                            group_name = "Scatterplots",
                            name = "Est. school-aged children dot-density" 
      ) %>%
      
      add_scatterplot_layer(data = sf_students_austin %>% ungroup() %>% arrange(school_short_name) ,
                            get_position = geometry,
                            get_fill_color = scale_color_category(col=school_short_name, 
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            #get_fill_color = ideacolors::idea_colors$lime,
                            radius_min_pixels = 2,
                            opacity = .5,
                            visible = FALSE,
                            group_name = "Scatterplots",
                            name = "Current IDEA Students"
      ) %>%
      
      add_scatterplot_layer(data = sf_schools_austin, 
                            get_position = geometry,
                            radius_min_pixels = 4,
                            get_fill_color = scale_color_category(col=school_short_name, 
                                                                  legend = FALSE,
                                                                  palette = ideacolors::idea_palettes$qual), 
                            tooltip = c(school_short_name),
                            pickable = TRUE,
                            name = "IDEA Schools",
                            group_name = "Schools"
      )  %>% 
      
      add_text_layer(data = sf_schools_austin, 
                     get_position = geometry,size_min_pixels = 13, size_max_pixels = 14,get_text_anchor = "end",
                     #radius_min_pixels = 4,
                     get_text = school_short_name,
                     get_color =  scale_color_category(col=school_short_name, 
                                                       legend = FALSE,
                                                       palette = ideacolors::idea_palettes$qual), 
                     tooltip = c(school_short_name),
                     pickable = FALSE,
                     name = "IDEA Schools",
                     group_name = "Schools"
      ) 
  )
}



## Run the application 
shinyApp(ui = ui, server = server)