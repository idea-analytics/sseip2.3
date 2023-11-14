# TODO: arrange helps, but doesn't put levels in the order we want in legend
# TODO: poverty scatterplot layer seems high
# TODO: widen the legend (mobility levels do not fit)
# TODO: modify labels in legend
# TODO: order the dot densities should be in and what is default "on"
# TODO: names of dot density maps (on app)
# TODO: Duval County, Children by Age (block), kids in water
# TODO: maybe move layers and legend off the map to see the map more? (if we can)
# TODO: Is there a way to select, say a particular age group, within the legend?

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
library(viridis)

# load mvp data
load(here::here("etl", "sf_dot_density_mvp.rda"))
load(here::here("etl", "sf_isochrones_idea_mvp.rda"))
load(here::here("etl", "sf_schools_idea_mvp.rda"))
#load(here::here("etl", "sf_students_idea_mvp.rda"))

sf_dot_density_mvp <- st_as_sf(sf_dot_density_mvp)
sf_isochrones_idea_mvp <- st_as_sf(sf_isochrones_idea_mvp) # polygons
sf_schools_idea_mvp <- st_as_sf(sf_schools_idea_mvp) # scatterplot + make larger dots with school name next to them
#sf_students_idea_mvp <- st_as_sf(sf_students_idea_mvp)

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

# school count (for use with color palette)
n_schools <- sf_schools_idea_mvp %>%
  distinct(school_short_name) %>%
  summarize(count = n())

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
    )

    %>%

      add_scatterplot_layer(data = sf_schools_idea_mvp,
                            get_position = geometry,
                            radius_min_pixels = 4,
                            get_fill_color = scale_color_category(col = school_short_name,
                                                                  legend = FALSE,
                                                                  palette = viridis(n_schools)),
                                                                  #palette = ideacolors::idea_palettes$qual), # not enough
                            tooltip = c(school_short_name),
                            pickable = TRUE,
                            name = "IDEA Schools",
                            group_name = "Schools"
                            )

    %>%

      add_text_layer(data = sf_schools_idea_mvp,
                     get_position = geometry,
                     size_min_pixels = 13,
                     size_max_pixels = 14,
                     get_text_anchor = "end",
                     #radius_min_pixels = 4,
                     get_text = school_short_name,
                     get_color =  scale_color_category(col = school_short_name,
                                                       legend = FALSE,
                                                       palette = viridis(n_schools)),
                                                       #palette = ideacolors::idea_palettes$qual), # not enough
                     tooltip = c(school_short_name),
                     pickable = FALSE,
                     name = "IDEA Schools",
                     group_name = "Schools"
                     )

    %>%

      add_scatterplot_layer(data = sf_children_by_age_block %>% arrange(variable),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Dot Density",
                            name = "Children by Age (Block)"
      )

    %>%

      add_scatterplot_layer(data = sf_children_by_age_tract %>% arrange(variable),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$div),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Dot Density",
                            name = "Children by Age (Tract)"
      )

    %>%

      add_scatterplot_layer(data = sf_children_in_poverty %>%
                              filter(!variable %in% c("Total",
                                                      "Income in the past 12 months below the poverty level",
                                                      "Enrolled in school",
                                                      "Enrolled in college undergraduate years",
                                                      "Enrolled in graduate or professional school",
                                                      "Not enrolled in school")) %>%
                              arrange(variable),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$greenorange),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Dot Density",
                            name = "Children in Poverty"
      )

    %>%

      add_scatterplot_layer(data = sf_household_income %>% filter(variable != "Total") %>% arrange(variable),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Dot Density",
                            name = "Household Income"
                            )

    %>%

      add_scatterplot_layer(data = sf_households_under18,
                            get_position = geometry,
                            get_fill_color = ideacolors::idea_colors$melon,
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Dot Density",
                            name = "Households with Children Under 18"
      )

    %>%

      add_scatterplot_layer(data = sf_mobility %>% filter(variable != "Total") %>% arrange(variable),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$blueorange),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Dot Density",
                            name = "Mobility"
      )

    %>%

      add_scatterplot_layer(data = sf_students_in_poverty,
                          get_position = geometry,
                          get_fill_color = "#9A95A2",
                          radius_min_pixels = 2,
                          opacity = .15,
                          visible = FALSE,
                          group_name = "Dot Density",
                          name = "Students in Poverty"
                          )

    %>%

    add_polygon_layer(data = sf_isochrones_idea_mvp,
                      get_polygon = geometry,
                      opacity = .05,
                      get_fill_color = scale_color_category(col = school_short_name,
                                                            legend = FALSE,
                                                            palette = viridis(n_schools)),
                                                            #palette = ideacolors::idea_palettes$qual), # too few
                      name = "Isochrones",
                      #group_name = "Schools",
                      visible = FALSE,
    )

    # %>%
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

  )
}



## Run the application
shinyApp(ui = ui, server = server)
