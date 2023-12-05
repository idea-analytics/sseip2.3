# 12/5 meeting:
# - review hex colors
# - any other modifications for MVP

# TODO 12/5:

# fix Travis coordinates
#

# TODO
# make school dots a star or something else
# try to include school selection in dropdown

# TODO: not sure if these are possible
# - widen the legend (mobility levels do not fit)
# - maybe move layers and legend off the map to see the map more? (if we can)
# - Is there a way to select, say a particular age group, within the legend?

# after MVP
# TODO: Round Rock, Parmer Park, Pflugerville labeled as Tarrant County somewhere? - Burleson was listed as Burleson County instead of Tarrant County, will update after Dec break
# TODO: mobility file missing county

# DONE
# drive times in bottom layer
# hex color
# - AUS: switch Kyle and Parmer Park
# -- switch Round Rock and Bluff Springs
# -- Round Rock green (#00BC00)

# hex codes, show
# show_col(idea_palette_ramp()(6))

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
  "TX", "Tarrant County", "Burleson County", # This is incorrect...Burleson County is the College Station area.
)

df_counties_ips <- tribble(
  ~ state, ~ region, ~ county,

  "OH", "Cincinnati", "Hamilton County",

  "FL", "Jacksonville", "Duval County",
  "FL", "Tampa", "Hillsborough County",
  "FL", "Tampa", "Polk County",

  "LA", "Southern Louisiana", "East Baton Rouge Parish",
)

df_counties_all <- bind_rows(
  df_counties_tx,
  df_counties_ips
)

# load mvp data
load("app/sf_dot_density_mvp.rda")
load("app/sf_isochrones_idea_mvp.rda")
load("app/sf_schools_idea_mvp.rda")
load("app/sf_students_idea_mvp.rda")

# read in school hex codes
hex_codes <- read.csv("app/school_hex_codes.csv")

sf_dot_density_mvp <- st_as_sf(sf_dot_density_mvp) %>%
  inner_join(df_counties_all %>% select(region, county), by = "county") %>%
  filter(county != "Burleson County")

sf_isochrones_idea_mvp <- st_as_sf(sf_isochrones_idea_mvp) %>%
  inner_join(df_counties_all %>% select(region, county), by = "county") %>%
  left_join(hex_codes %>% select(school_short_name, hex_code), by = "school_short_name") %>%
  filter(county != "Burleson County")

sf_schools_idea_mvp <- st_as_sf(sf_schools_idea_mvp) %>%
  left_join(hex_codes %>% select(school_short_name, hex_code), by = "school_short_name") %>%
  mutate(school_short_name = recode_factor(school_short_name, "IDEA Fairfield" = "Fairfield"))

sf_students_idea_mvp <- st_as_sf(sf_students_idea_mvp) %>%
  rename(region = region_description) %>%
  left_join(hex_codes %>% select(school_short_name, hex_code), by = "school_short_name")

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

## mapbox token

options(rdeck.mapbox_access_token = Sys.getenv("MAPBOX_API_TOKEN"))

## Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Site Location Explorer"),

  # set up side panel with multiple options
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "region_name",
                  label = "Select your region of interest",
                  choices = sort(unique(sf_schools_idea_mvp$region))
                  )
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
          initial_bounds = sf_household_income %>%
            filter(region == input$region_name),
          theme = "light",
          editor = FALSE,
          blending_mode = "subtractive"
    )

    %>%

    #   # Mobility
    #   add_scatterplot_layer(data = sf_mobility %>% filter(variable != "Total") %>% arrange(variable),
    #                         get_position = geometry,
    #                         get_fill_color = scale_color_category(col = variable,
    #                                                               legend = TRUE,
    #                                                               palette = ideacolors::idea_palettes$blueorange),
    #                         radius_min_pixels = 2,
    #                         opacity = .15,
    #                         visible = FALSE,
    #                         group_name = "Mobility",
    #                         #name = "Mobility"
    #   )
    #
    # %>%

      # Students in Poverty
      add_scatterplot_layer(data = sf_students_in_poverty %>%
                              filter(!variable %in% c("Total",
                                                      "Income in the past 12 months below the poverty level",
                                                      "Enrolled in school",
                                                      "Enrolled in college undergraduate years",
                                                      "Enrolled in graduate or professional school",
                                                      "Not enrolled in school")) %>%
                              mutate(variable = as.factor(variable)) %>%
                              # rename variable levels so they fit in legend
                              mutate(variable_new = case_when(variable == "Enrolled in nursery school, preschool" ~ "Enrolled in nursery/PK",
                                                              variable == "Enrolled in kindergarten" ~ "Enrolled in kindergarten",
                                                              variable == "Enrolled in grade 1 to grade 4" ~ "Enrolled in grades 1-4",
                                                              variable == "Enrolled in grade 5 to grade 8" ~ "Enrolled in grades 5-8",
                                                              variable == "Enrolled in grade 9 to grade 12" ~ "Enrolled in grades 9-12",
                                                              variable == "Not enrolled in school" ~ "Not enrolled in school")) %>%
                              mutate(variable_new = ordered(variable_new, levels = c("Enrolled in nursery/PK",
                                                                                     "Enrolled in kindergarten",
                                                                                     "Enrolled in grades 1-4",
                                                                                     "Enrolled in grades 5-8",
                                                                                     "Enrolled in grades 9-12",
                                                                                     "Not enrolled in school"))),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable_new,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            #get_fill_color = "#9A95A2",
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Income",
                            name = "Students in Poverty"
                            )

    %>%

      # Household Income
      add_scatterplot_layer(data = sf_household_income %>%
                              filter(variable != "Total") %>%
                              mutate(variable = as.factor(variable)) %>%
                              mutate(variable = ordered(variable, levels = c("Less than $20,000",
                                                                             "$20,000 to $29,999",
                                                                             "$30,000 to $39,999",
                                                                             "$40,000 to $49,999",
                                                                             "$50,000 to $59,999",
                                                                             "$60,000 to $74,999",
                                                                             "$75,000 to $99,999",
                                                                             "$100,000 or more"))),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Income",
                            name = "Household Income"
      )

    %>%

      # Children in Poverty
      add_scatterplot_layer(data = sf_children_in_poverty %>%
                              mutate(variable = as.factor(variable)) %>%
                              mutate(variable_new = case_when(variable == "Under 1.00" ~ "< 1X poverty",
                                                              variable == "1.00 to 1.99" ~ "1X-2X poverty")) %>%
                              mutate(variable_new = ordered(variable_new, levels = c("< 1X poverty",
                                                                                     "1X-2X poverty"))),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable_new,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$greenorange),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Income",
                            name = "Children in Poverty"
      )

    %>%

      # Households with Children Under 18
      add_scatterplot_layer(data = sf_households_under18,
                            get_position = geometry,
                            get_fill_color = ideacolors::idea_colors$melon,
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Population of Children",
                            name = "Households with Children Under 18"
      )

    %>%

      # Children by Age (Tract)
      add_scatterplot_layer(data = sf_children_by_age_tract %>%
                              mutate(variable = as.factor(variable)) %>%
                              mutate(variable = ordered(variable, levels = c("Under 3 years",
                                                                             "3 and 4 years",
                                                                             "5 years",
                                                                             "6 to 8 years",
                                                                             "9 to 11 years",
                                                                             "12 to 14 years",
                                                                             "15 to 17 years"))),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$div),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Population of Children",
                            name = "Children by Age (Tract)"
      )

    %>%

      # Children by Age (Block)
      add_scatterplot_layer(data = sf_children_by_age_block %>%
                              mutate(variable = as.factor(variable)) %>%
                              mutate(variable = ordered(variable, levels = c("Under 5 years",
                                                                             "5 to 9 years",
                                                                             "10 to 14 years",
                                                                             "15 to 17 years"))),
                            get_position = geometry,
                            get_fill_color = scale_color_category(col = variable,
                                                                  legend = TRUE,
                                                                  palette = ideacolors::idea_palettes$qual),
                            radius_min_pixels = 2,
                            opacity = .15,
                            visible = FALSE,
                            group_name = "Population of Children",
                            name = "Children by Age (Block)"
      )

    %>%

    # Current IDEA students
    add_scatterplot_layer(data = sf_students_idea_mvp %>%
                            arrange(school_short_name),
                          get_position = geometry,
                          get_fill_color = hex_code,
                          radius_min_pixels = 2,
                          opacity = .15,
                          visible = FALSE,
                          group_name = "IDEA",
                          name = "Current Students"
    )

    %>%

      # IDEA Schools
      add_scatterplot_layer(data = sf_schools_idea_mvp %>%
                              arrange(school_short_name),
                            get_position = geometry,
                            radius_min_pixels = 4,
                            get_fill_color = hex_code,
                            tooltip = c(school_short_name), # defines which columns are displayed when hovered over
                            pickable = TRUE, # allows for the tooltip names to be shown when hovered over
                            group_name = "IDEA",
                            name = "Schools"
      )

    %>%

    # IDEA School Short Name
    add_text_layer(data = sf_schools_idea_mvp %>%
                     arrange(school_short_name),
                   get_position = geometry,
                   size_min_pixels = 13,
                   size_max_pixels = 14,
                   get_text_anchor = "end",
                   # radius_min_pixels = 4,
                   get_text = school_short_name,
                   # get_color = scale_color_category(col = school_short_name,
                   #                                  legend = FALSE,
                   #                                  #palette = turbo(n_schools)), # turbo is part of the viridis package
                   #                                  palette = ideacolors::idea_palettes$qual), # not enough
                   get_color = hex_code,
                   group_name = "IDEA",
                   name = "Schools"
    )

    %>%

      # Isochrones (drive distance)
      add_polygon_layer(data = sf_isochrones_idea_mvp,
                        get_polygon = geometry,
                        opacity = .01,
                        get_fill_color = hex_code,
                        name = "Drive Time Radius",
                        group_name = "IDEA",
                        visible = FALSE,
      )
  )
  }

## Run the application
shinyApp(ui = ui, server = server)
