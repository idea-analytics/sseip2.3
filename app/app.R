# TODO: arrange helps, but doesn't put levels in the order we want in legend: use factor level sort
# TODO: color palette by region -- add color for each school in .csv then recreate sf file -- use that variable for the colors
# TODO: widen the legend (mobility levels do not fit)
# TODO: modify labels in legend
# TODO: order the dot densities should be in and what is default "on"
# TODO: maybe move layers and legend off the map to see the map more? (if we can)
# TODO: Is there a way to select, say a particular age group, within the legend?

# load libraries

library(tidyverse)
library(sf)
# devtools::install_github("qfes/rdeck")
library(rdeck)
# library(tidycensus)
sf::sf_use_s2(FALSE)
library(shiny)
# library(mapboxapi)
# library(here)
# install.packages("devtools")
# devtools::install_github("idea-analytics/ideacolors")
library(ideacolors)
library(viridis)

## debug
# options(shiny.fullstacktrace = TRUE) # writes the full trace of steps taken by Shiny to the console
# options(shiny.error = browser) # identifies the code chunk where the error occurs

# get regions
df_counties_tx <- tribble(
  ~ state, ~ region, ~ county,

  "TX", "Austin", "Travis County",
  "TX", "Austin", "Hays County",
  "TX", "Austin", "Bastrop County",
  "TX", "Austin", "Williamson County",

  "TX", "El Paso", "El Paso County",

  "TX", "Greater Houston Area", "Harris County",
  "TX", "Greater Houston Area", "Montgomery County",

  "TX", "Permian Basin", "Midland County",
  "TX", "Permian Basin", "Ector County",

  "TX", "Rio Grande Valley", "Cameron County",
  "TX", "Rio Grande Valley", "Hidalgo County",
  "TX", "Rio Grande Valley", "Starr County",

  "TX", "San Antonio", "Bexar County",
  "TX", "San Antonio", "Comal County",

  "TX", "Tarrant County", "Tarrant County",
  "TX", "Tarrant County", "Parker County",
  "TX", "Tarrant County", "Burleson County",
)

df_counties_ips <- tribble(
  ~ state, ~ region, ~ county,

  "OH", "Cincinnati", "Hamilton County",

  "FL", "Jacksonville", "Duval County",
  "FL", "Tampa", "Hillsborough County",
  "FL", "Tampa", "Polk County" ,

  "LA", "Southern Louisiana", "East Baton Rouge Parish",
)

df_counties_all <- bind_rows(
  df_counties_tx,
  df_counties_ips
)

# load mvp data
load("sf_dot_density_mvp.rda")
load("sf_isochrones_idea_mvp.rda")
load("sf_schools_idea_mvp.rda")
load("sf_students_idea_mvp.rda")

sf_dot_density_mvp <- st_as_sf(sf_dot_density_mvp) %>%
  inner_join(df_counties_all %>% select(region, county), by = "county")

sf_isochrones_idea_mvp <- st_as_sf(sf_isochrones_idea_mvp) %>%
  inner_join(df_counties_all %>% select(region, county), by = "county")

sf_schools_idea_mvp <- st_as_sf(sf_schools_idea_mvp)

sf_students_idea_mvp <- st_as_sf(sf_students_idea_mvp)

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

# sf_mobility <- sf_dot_density_mvp %>%
#   filter(table_short_name == "Mobility",
#          geography == "tract")

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
      selectInput(inputId = "region_name",
                  label = "Select your region of interest",
                  choices = c("Austin",
                              "Cincinnati",
                              "Jacksonville",
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
          initial_bounds = sf_schools_idea_mvp %>% filter(region == input$region_name),
          theme = "light",
          editor = FALSE,
          blending_mode = "subtractive" #,
          #height = 800
    )

    %>%

      add_scatterplot_layer(data = sf_schools_idea_mvp,
                            get_position = geometry,
                            radius_min_pixels = 4,
                            get_fill_color = scale_color_category(col = school_short_name,
                                                                  legend = FALSE,
                                                                  palette = turbo(n_schools)), # turbo is part of the viridis package
                                                                  #palette = ideacolors::idea_palettes$qual), # not enough
                            tooltip = c(school_short_name), # defines which columns are displayed when hovered over
                            pickable = TRUE, # allows for the tooltip names to be shown when hovered over
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
                     get_color = scale_color_category(col = school_short_name,
                                                      legend = FALSE,
                                                      palette = turbo(n_schools)), # turbo is part of the viridis package
                                                      #palette = ideacolors::idea_palettes$qual), # not enough
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

      add_scatterplot_layer(data = sf_children_in_poverty %>% arrange(variable),
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

    #   add_scatterplot_layer(data = sf_mobility %>% filter(variable != "Total") %>% arrange(variable),
    #                         get_position = geometry,
    #                         get_fill_color = scale_color_category(col = variable,
    #                                                               legend = TRUE,
    #                                                               palette = ideacolors::idea_palettes$blueorange),
    #                         radius_min_pixels = 2,
    #                         opacity = .15,
    #                         visible = FALSE,
    #                         group_name = "Dot Density",
    #                         name = "Mobility"
    #   )
    #
    # %>%

      add_scatterplot_layer(data = sf_students_in_poverty %>%
                              filter(!variable %in% c("Total",
                                                      "Income in the past 12 months below the poverty level",
                                                      "Enrolled in school",
                                                      "Enrolled in college undergraduate years",
                                                      "Enrolled in graduate or professional school",
                                                      "Not enrolled in school")),
                          get_position = geometry,
                          get_fill_color = scale_color_category(col = variable,
                                                                legend = TRUE,
                                                                palette = ideacolors::idea_palettes$blueorange), # find another palette
                          #get_fill_color = "#9A95A2",
                          radius_min_pixels = 2,
                          opacity = .15,
                          visible = FALSE,
                          group_name = "Dot Density",
                          name = "Students in Poverty"
                          )

    %>%

    add_polygon_layer(data = sf_isochrones_idea_mvp,
                      get_polygon = geometry,
                      opacity = .01,
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
