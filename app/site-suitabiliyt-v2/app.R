#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Packages -------------------------------------------------------------------
library(shiny)
library(bslib)
library(sfarrow)
library(mapgl)
library(tidyverse)
library(ideacolors)

# Data -----------------------------------------------------------------------
sf_site_suitability_mvp <- st_read_feather("sf_site_suitability.feather") %>%
  filter(!is.na(h3_address)) %>%
  mutate(popup = glue::glue("<strong>Site Suitability Index: {round(index_total,2)}</strong>",
                            "Students in poverty index: {round(index_students_poverty,2)}",
                            "Low quality schools count: {round(total_low_quality_nn_schools,2)}",
                            "High quality schools count: {round(total_high_quality_nn_schools,2)}",
                            .sep = "<br>"),
         tooltip = glue::glue("SSI {round(index_total,2)}")
  )

regions <- sf_site_suitability_mvp %>%
  as_tibble() %>%
  dplyr::select(REGION) %>%
  dplyr::distinct() %>%
  arrange(REGION)




sf_dot_density_mvp <- st_read_feather("sf_dot_density_mvp.feather")

hex_codes <- read.csv("school_hex_codes.csv")
sf_isochrones_idea_mvp <- st_read_feather("sf_isochrones_idea_mvp.feather") %>%
  inner_join(hex_codes, by = c("school_short_name" = "school_short_name"))

sf_idea_schools_mvp <- st_read_feather("sf_schools_idea_mvp.feather") %>%
  inner_join(hex_codes, by = c("school_short_name" = "school_short_name"))

sf_idea_students_mvp <- st_read_feather("sf_students_idea_mvp.feather") %>%
  inner_join(hex_codes, by = c("school_short_name" = "school_short_name"))



sf_household_income <- sf_dot_density_mvp %>%
  filter(table_short_name == "Household income",
         geography == "block group")

sf_students_in_poverty <- sf_dot_density_mvp %>%
  filter(table_short_name == "Students in poverty",
         geography == "block group",
         !variable %in% c("Total",
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
                                                         "Not enrolled in school")))

sf_households_under18 <- sf_dot_density_mvp %>%
  filter(table_short_name == "Households with children under 18",
         geography == "block group")

# sf_children_by_age_tract <- sf_dot_density_mvp %>%
#   filter(table_short_name == "Children by age group",
#          geography == "tract")

sf_children_by_age_block_group <- sf_dot_density_mvp %>%
  filter(table_short_name == "Children by age group",
         geography == "block group")

sf_children_in_poverty <- sf_dot_density_mvp %>%
  filter(table_short_name == "Children in poverty",
         geography == "tract")

# Palettes -------------------------------------------------------------------
children_by_age_block_palette <- c("#a1d99b",
                                   "#41ab5d",
                                   "#238b45",
                                   "#006d2c")

children_in_poverty_palette <- c("#969696",
                                 "#525252")


students_in_poverty_palette <- c("#c7e9b4",
                                 "#7fcdbb",
                                 "#41b6c4",
                                 "#1d91c0",
                                 "#225ea8",
                                 "#253494")

household_income_palette <- c("#fde0dd",
                              "#fcc5c0",
                              "#fa9fb5",
                              "#f768a1",
                              "#dd3497",
                              "#ae017e",
                              "#7a0177",
                              "#49006a") #

# UI ------------------------------------------------------------------------
ui <- page_sidebar(

    title = "Site Suitability Analysis",
    sidebar = sidebar(
      title = "Layer controls",

      ## Region selector ------------------------------------------------------
      selectizeInput(
        "region_selector",
        label = "Select region",
        choices = regions$REGION,
        selected = "Austin"
      ),

      # SSI selector ---------------------------------------------------------
      input_switch(
        id = "ssi_switch",
        label = "Show site suitability index",
        value = TRUE

        ),
      # Layer selector -------------------------------------------------------
      checkboxGroupInput(
        "layer_selector",
        label = "Select layer to display",
        choices = list(
            "IDEA Students" = "idea_stus",
            "Drive times" = "drive_times",
            "Children by age" = "children_by_age",
            "Households with children under 18" = "households_children_under18",
            "Children in Poverty" = "children_in_poverty",
            "Household Income" = "household_income",
            "Students in Poverty" = "students_in_poverty"),
        selected = NULL
      ),
      #hline(),
      #hide(filter_input())
      # selectizeInput(
      #    "layer_selector",
      #   label = "Select layer to display",
      #   choices = list(#"Choose Layer" ="",
      #                 `IDEA`        = list(
      #                                     "IDEA Students" = "idea_stus",
      #                                      "Drive times" = "drive_times"),
      #                 `Population` =list("Children by age" = "children_by_age",
      #                                     "Households with children under 18" = "households_with_children_under_18"),
      #                 `Poverty`    =   list("Children in Poverty" = "children_in_poverty",
      #                                     "Household Income" = "household_income",
      #                                     "Students in Poverty" = "students_in_poverty")),
      #   selected = NULL
      #   ),
      selectizeInput(
        "map_type_selector",
        label = "Select map type:",
        choices = c(
          "Frankfurt" = "frankfurt",
          "Standard" = "standard",
          "Streets" = "streets",
          "Outdoors" = "outdoors",
          "Light" = "light",
          "Dark" = "dark",
          "Satellite" = "satellite"
        ),
        selected = "frankfurt"
      )

      #verbatimTextOutput("clicked_feature")
      ),
    ## Main body --------------------------------------------------------------


    card(
      title = "Map",
      mapboxglOutput("map")
      )

)

# Server ----------------------
server <- function(input, output) {

  # output$clicked_feature <- renderPrint({
  #   req(input$map_feature_click)
  #   input$map_feature_click
  # })

  ## Initial map --------------------------------------------------------------
  output$map <- renderMapboxgl({
    #req(input$region_selector)


      mapboxgl(center = c(-97.661324953909, 30.3381543635444),
               zoom = 8.28974152189369,
               style = rdeck::mapbox_gallery_frank()) %>%
      add_fill_layer(id = "suitability",
                     source = sf_site_suitability_mvp,
                     fill_color = interpolate(column = "index_total",
                                              values = c(.001, .01, .1, 1),
                                              stops  = c(idea_colors$lightgray,
                                                         idea_colors$gray,
                                                         idea_colors$melon,
                                                         idea_colors$magenta),
                                              na_color = idea_colors$coolgray),
                     fill_opacity = 0.4,
                     popup = "popup",
                     tooltip = "tooltip",
                     hover_options = list(
                       fill_color = "yellow",
                       fill_opacity = .8
                     )
      ) %>%
      add_legend(
        "Site Suitability Index",
        position = "bottom-left",
        values = c(.001, .01, .1, 1),
        colors  = c(idea_colors$lightgray,
                    idea_colors$gray,
                    idea_colors$melon,
                    idea_colors$magenta)
      ) %>%

      add_fill_layer(
        id = "drive_times",
        source = sf_isochrones_idea_mvp,
        fill_color = get_column("hex_code"),
        fill_opacity = 0.4,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id ="schools",
        source = sf_idea_schools_mvp,slot = "top",
        circle_radius = 5,
        circle_color = get_column("hex_code"),
        circle_opacity = 1, tooltip = "school_short_name") %>%

      add_circle_layer(
        id = "students-layer",
        source = sf_idea_students_mvp,
        circle_radius = 2.5,
        circle_color = get_column("hex_code"),
        circle_opacity = .65,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "children_by_age",
        source = sf_children_by_age_block_group,
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years"),
          stops = children_by_age_block_palette),
        circle_opacity = .45,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "children_in_poverty",
        source = sf_children_in_poverty,
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("Under 1.00", "1.00 to 1.99"),
          stops = children_in_poverty_palette),
        circle_opacity = .5,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "households_children_under18",
        source = sf_households_under18,
        circle_radius = 2,
        circle_color = "#626363",
        circle_opacity = .333,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "students_in_poverty",
        source = sf_students_in_poverty,
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("nrolled in nursery school, preschool",
                     "Enrolled in kindergarten",
                     "Enrolled in grade 1 to grade 4",
                     "Enrolled in grade 5 to grade 8",
                     "Enrolled in grade 9 to grade 12"),
          stops = students_in_poverty_palette[1:5]),
        circle_opacity = .333,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "household_income",
        source = sf_household_income %>% filter(variable != "Total"),
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("Less than $20,000",
                     "$20,000 to $29,999",
                     "$30,000 to $39,999",
                     "$40,000 to $49,999",
                     "$50,000 to $59,999",
                     "$60,000 to $74,999",
                     "$75,000 to $99,999",
                     "$100,000 or more"),
          stops = idea_palette_ramp("blueorange")(8)),
        circle_opacity = .333,
        visibility = "none"
      )# %>% add_layers_control(position = "bottom-right", collapsible = TRUE
                              # ) #%>%
      #fit_bounds(sf_site_suitability_mvp %>% filter(REGION=="Austin"), animate = T) #%>%
  })

  ## ssi layer observer ------------------------------------------------------
  observeEvent(input$ssi_switch, {
    #req(input$ssi_switch)
    if(input$ssi_switch == FALSE){
      mapboxgl_proxy("map") %>%
        #clear_layer("suitability")
        set_layout_property("suitability", "visibility", "none")

    } else {
    #if(input$ssi_switch == TRUE){
      mapboxgl_proxy("map") %>%
        set_layout_property("suitability", "visibility", "visible")

    }

  })


  ## region event observer ---------------------------------------------------
  observeEvent(input$region_selector,
          #, ignoreNULL = TRUE, ignoreInit = FALSE,
          {
    req(input$region_selector)
    mapboxgl_proxy("map") %>%
      fit_bounds(sf_site_suitability_mvp %>% filter(REGION==input$region_selector),
                 animate = TRUE)
  }) #%>% bindEvent(input$region_selector)

  #map type observer ---------------------------------------------------------
  observeEvent(input$map_type_selector, {
    #req(input$region_selector)
    if (input$map_type_selector != "frankfurt") {
      mapboxgl_proxy("map")  %>%
        #set_view(input$map_center, input$map_zoom) %>%
        set_style(mapbox_style(input$map_type_selector), diff = FALSE) %>%
        fit_bounds(sf_site_suitability_mvp %>% filter(REGION==input$region_selector),
                   animate = FALSE)

    } else {
      mapboxgl_proxy("map") %>%
        set_style(style = "mapbox://styles/mapbox-map-design/ckshxkppe0gge18nz20i0nrwq", diff = FALSE) %>%
        set_view(input$map_center, input$map_zoom)
    }

  })

  ## layer selector observer -------------------------------------------------
  observeEvent(input$layer_selector, {
    ### IDEA Students --------------------------------------------------------
    if("idea_stus" %in% input$layer_selector ){
    mapboxgl_proxy("map") %>%
      set_layout_property("students-layer", "visibility",  "visible")
    } else {
    #if({"idea_stus" %in% input$layer_selector}){
      mapboxgl_proxy("map") %>%
        set_layout_property("students-layer", "visibility", "none")
    }

    ### Drive times ----------------------------------------------------------
    if("drive_times" %in% input$layer_selector){
      mapboxgl_proxy("map") %>%
        set_layout_property("drive_times", "visibility", "visible")
    } else {
       mapboxgl_proxy("map") %>%
         set_layout_property("drive_times", "visibility",  "none")
    }

    ### Children by age -----------------------------------------------------
    if("children_by_age" %in% input$layer_selector){
      mapboxgl_proxy("map") %>%
        set_layout_property("children_by_age", "visibility", "visible")
    } else {
      mapboxgl_proxy("map") %>%
        set_layout_property("children_by_age", "visibility",  "none")
    }

    ### Children in poverty ---------------------------------------------------
    if("children_in_poverty" %in% input$layer_selector){
      mapboxgl_proxy("map") %>%
        set_layout_property("children_in_poverty", "visibility", "visible")
    } else {
      mapboxgl_proxy("map") %>%
        set_layout_property("children_in_poverty", "visibility",  "none")
    }

    ### Households children under 18 -----------------------------------------
    if("households_children_under18" %in% input$layer_selector){
      mapboxgl_proxy("map") %>%
        set_layout_property("households_children_under18", "visibility", "visible")
    } else {
      mapboxgl_proxy("map") %>%
        set_layout_property("households_children_under18", "visibility",  "none")
    }

    ### Students in poverty ---------------------------------------------------
    if("students_in_poverty" %in% input$layer_selector){
      mapboxgl_proxy("map") %>%
        set_layout_property("students_in_poverty", "visibility", "visible")
    } else {
      mapboxgl_proxy("map") %>%
        set_layout_property("students_in_poverty", "visibility",  "none")
    }

    ### Household income ---------------------------------------------------
    if("household_income" %in% input$layer_selector){
      mapboxgl_proxy("map") %>%
        set_layout_property("household_income", "visibility", "visible")
    } else {
      mapboxgl_proxy("map") %>%
        set_layout_property("household_income", "visibility",  "none")
    }

  },
  ignoreNULL = FALSE
  )
}
# Run the application
shinyApp(ui = ui, server = server)
