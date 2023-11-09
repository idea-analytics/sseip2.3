# TODO: palette does not have enough colors
# TODO: there is a "Total" in the variable column (165122/330223 rows of the data frame)
# TODO: arrange helps, but doesn't put it in the order we want

# load libraries

library(tidyverse)
library(sf)
# devtools::install_github("qfes/rdeck")
library(rdeck)
library(tidycensus)
sf::sf_use_s2(FALSE)
library(shiny)
library(mapboxapi)
library(here)
# install.packages("devtools")
# devtools::install_github("idea-analytics/ideacolors")
library(ideacolors)

# load mvp data
load(here::here("etl", "sf_dot_density_mvp.rda"))

sf_dot_density_mvp <- st_as_sf(sf_dot_density_mvp)

# create separate data frames for block groups and tracts

sf_household_income <- sf_dot_density_mvp %>%
  filter(table_short_name == "Household income",
         geography == "block group")

sf_students_in_poverty <- sf_dot_density_mvp %>%
  filter(table_short_name == "Students in poverty",
         geography == "block group")

sf_households_under18 <- sf_dot_density_mvp %>%
  filter(table_short_name == "Households with children under 18",
         geography == "block group")

sf_mobility <- sf_dot_density_mvp %>%
  filter(table_short_name == "Mobility",
         geography == "tract")

sf_children_by_age_tract <- sf_dot_density_mvp %>%
  filter(table_short_name == "Children by age group",
         geography == "tract")

sf_children_by_age_block <- sf_dot_density_mvp %>%
  filter(table_short_name == "Children by age group",
         geography == "block group")

sf_children_in_poverty <- sf_dot_density_mvp %>%
  filter(table_short_name == "Children in poverty",
         geography == "tract")

# ## load data
#
# # simple frame school locations
# load("sf_schools_austin.RData")
# #load(here::here("sf_schools_austin.RData"))
# # simple frame student locations
# load("sf_students_austin.RData")
# # block_group_children
# load("pp_children_bg_dots.Rdata")
# # drive_times
# load("pp_site_drive_15_min_sf.Rdata")

## mapbox token

options(rdeck.mapbox_access_token = Sys.getenv("MAPBOX_API_TOKEN"))

## Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Our Cool Title"),

  # set up side panel with multiple options
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "region",
                  label = "Select your region of interest",
                  choices = c("Travis County",
                              "Duval County",
                              "Polk County",
                              "Austin",
                              "Jacksonville",
                              "Lakeland",
                              "Ohio",
                              "Permian Basin",
                              "Tampa",
                              "Tarrant County"
                              )),
    ),

    mainPanel(
      rdeckOutput("map")
    )
)
)

## Define server logic required to make visuals

server <- function(input, output) {

  output$map <- renderRdeck(

    rdeck(map_style = mapbox_gallery_frank(),
          initial_bounds = sf_household_income %>% filter(county == input$region),
          theme = "light",
          editor = FALSE,
          blending_mode = "subtractive",
          height = 800
    ) %>%

      # add_polygon_layer(data = pp_site_drive_15_min_sf %>% filter(time == 10),
      #                   get_polygon = geometry, opacity = .05,
      #                   get_fill_color = scale_color_category(col=school_short_name,
      #                                                         legend = FALSE,
      #                                                         palette = ideacolors::idea_palettes$qual),
      #                   name = "10-min drivetime",
      #                   group_name = "Schools",
      #                   visible = FALSE,
      # ) %>%
      #
      # add_heatmap_layer(data = sf_students_austin,
      #                   get_position = geometry,
      #                   opacity = .25,
      #                   name = "Current IDEA student density",
      #                   group_name = "Heatmaps",
      #                   visible = FALSE
      # ) %>%
      #
      # add_heatmap_layer(data = pp_children_bg_dots,
      #                   get_position = geometry,
      #                   opacity = .4,
      #                   visible = FALSE,
      #                   color_range = viridisLite::turbo(6, direction = 1),
      #                   name = "Est. school-aged children density",
      #                   group_name = "Heatmaps",
      #                   threshold = .5
      # ) %>%
      #
      add_scatterplot_layer(data = sf_household_income %>% arrange(variable),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            # get_fill_color = "#9A95A2",
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = TRUE,
                            group_name = "Scatterplots",
                            name = "Household Income"
      )
    # %>%

      # add_scatterplot_layer(data = sf_students_austin %>% ungroup() %>% arrange(school_short_name) ,
      #                       get_position = geometry,
      #                       get_fill_color = scale_color_category(col=school_short_name,
      #                                                             legend = TRUE,
      #                                                             palette = ideacolors::idea_palettes$qual),
      #                       #get_fill_color = ideacolors::idea_colors$lime,
      #                       radius_min_pixels = 2,
      #                       opacity = .5,
      #                       visible = FALSE,
      #                       group_name = "Scatterplots",
      #                       name = "Current IDEA Students"
      # ) %>%
      #
      # add_scatterplot_layer(data = sf_schools_austin,
      #                       get_position = geometry,
      #                       radius_min_pixels = 4,
      #                       get_fill_color = scale_color_category(col=school_short_name,
      #                                                             legend = FALSE,
      #                                                             palette = ideacolors::idea_palettes$qual),
      #                       tooltip = c(school_short_name),
      #                       pickable = TRUE,
      #                       name = "IDEA Schools",
      #                       group_name = "Schools"
      # )  %>%
      #
      # add_text_layer(data = sf_schools_austin,
      #                get_position = geometry,size_min_pixels = 13, size_max_pixels = 14,get_text_anchor = "end",
      #                #radius_min_pixels = 4,
      #                get_text = school_short_name,
      #                get_color =  scale_color_category(col=school_short_name,
      #                                                  legend = FALSE,
      #                                                  palette = ideacolors::idea_palettes$qual),
      #                tooltip = c(school_short_name),
      #                pickable = FALSE,
      #                name = "IDEA Schools",
      #                group_name = "Schools"
      )
  #)
}



## Run the application
shinyApp(ui = ui, server = server)
