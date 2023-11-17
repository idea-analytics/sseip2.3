# load libraries

library(tidyverse)
library(sf)
library(rdeck)
library(tidycensus)
sf::sf_use_s2(FALSE)
library(shiny)
library(mapboxapi)

# set working directory

setwd("C:/Users/lindsey.smith/OneDrive - ideapublicschools.org/IDEA/Projects/202310a enrollment - GIS, census/proof of concept map/")

## load data

# simple frame school locations
load("sf_schools_austin.RData")
# simple frame student locations
load("sf_students_austin.RData")
# block_group_children
load("pp_children_bg_dots.Rdata")
# drive_times
load("pp_site_drive_15_min_sf.Rdata")

## mapbox token

options(rdeck.mapbox_access_token = 'pk.eyJ1IjoibHNtaXRoNzAwIiwiYSI6ImNsbmY0azMxOTBmdjQybXJpOGQ1bzAyeXkifQ.VKgE6_4p63rTuRo5QlHl5w')

## Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Austin Student Density App"),
  
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel(
  #     selectInput("school", "Choose a School",
  #                 choices = sf_schools_austin$school_short_name)
  #     ),
    
    # Show a plot of the generated distribution
    mainPanel(
      rdeckOutput("map")
      )
)



## Define server logic required to make map
server <- function(input, output) {
  
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