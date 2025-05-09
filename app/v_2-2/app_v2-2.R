# app v. 2.2.0

# Packages -------------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(sf)
library(sfarrow)
library(mapgl)
library(tidyverse)
library(ideacolors)
library(shinybusy)
library(capture)



# Data -----------------------------------------------------------------------

## 1. import datasets from disk
##    - df_counties_all: county-region lookup
##    - sf_site_suitability: suitability index values + hexagon geometries
##    - hex_codes: hex code-
##    - sf_isochrones_idea: 1-15 minute 8 AM Tuesday isochrones for IDEA schools
##    - sf_idea_schools: point geometries for IDEA schools
##    - sf_states: multipolygon geometries for TX, LA, FL, OH
##    - sf_counties: multipolygon geometries for TX, LA, FL, OH
##    - sf_school_districts_2022: multipolygon geometries for TX, LA, FL, OH
##    - sf_accountability_schools_tx, sf_accountability_schools_la,
##        sf_accountability_schools_fl, sf_accountability_schools_oh:
##        point geometries for accountability in TX, LA, FL, OH
##
## 2. calculated data - this is computed each time the app runs
##    - regions: small data frame of region names
##    - date: system date


df_counties_all <- read_csv("county_region_lookup.csv")

hex_codes <- read_csv("school_hex_codes.csv")

sf_site_suitability <- st_read_parquet("sf_index_2025-05-06.parquet") %>%
  filter(!is.na(h3_address)) %>%
  mutate(
    popup = glue::glue(
      "<strong>Site Suitability Index: {round(index_total,2)}</strong>",
      "Students in poverty index: {round(index_students_poverty,2)}",
      "Low quality schools count: {round(total_low_quality_nn_schools,2)}",
      "High quality schools count: {round(total_high_quality_nn_schools,2)}",
      .sep = "<br>"),
    tooltip = glue::glue("SSI {round(index_total,2)}")
  )

sf_isochrones_idea <- st_read_parquet("idea_isochrones_2025-05-04.parquet") %>%
  inner_join(hex_codes, by = c("SchoolShortName" = "school_short_name"))

sf_idea_schools <- st_read_parquet("idea_schools_2025-05-04.parquet") %>%
  inner_join(hex_codes, by = c("SchoolShortName" = "school_short_name"))

sf_states <- st_read_parquet("states_2021.parquet")

sf_counties <- st_read_parquet("counties_2023.parquet") %>%
  left_join(sf_states %>%
              as.data.frame() %>%
              select(STATEFP,
                     STUSPS,
                     STATENAME = NAME),
            by = c("STATEFP" = "STATEFP")) %>%
  left_join(df_counties_all,
            by = c("NAMELSAD" = "county",
                   "STUSPS" = "st_usps"))

sf_school_districts_2022 <- st_read_parquet("school_districts_2023.parquet") %>%
  left_join(sf_states %>%
              as.data.frame() %>%
              select(STATEFP,
                     STUSPS,
                     STATENAME = NAME),
            by = c("STATEFP" = "STATEFP"))

sf_accountability_schools_tx <- st_read_parquet("sf_accountability_tx_schools_2023.parquet")
sf_accountability_schools_la <- st_read_parquet("sf_accountability_la_2024.parquet") %>%
  mutate(sps = as.numeric(sps))
sf_accountability_schools_fl <- st_read_parquet("sf_accountability_fl_2024.parquet") %>%
  mutate(pi = as.numeric(pi))
sf_accountability_schools_oh <- st_read_parquet("sf_accountability_oh_schools_2024.parquet")

regions <- sf_counties %>%
  as_tibble() %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::select(region) %>%
  dplyr::distinct() %>%
  arrange(region)

date <- Sys.Date()


# UI themes --------------------------------------------------------------------

## build the themeing and palettes for the app

theme_idea_light <- bs_theme(bootswatch = "yeti") %>%
  bs_theme_update(primary = "#002F6C")

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
                                 "#225ea8")
                                 # "#253494")

household_income_palette <- c("#fde0dd",
                              "#fcc5c0",
                              "#fa9fb5",
                              "#f768a1",
                              "#dd3497",
                              "#ae017e",
                              "#7a0177",
                              "#49006a") #

# UI ---------------------------------------------------------------------------
ui <- page_navbar(

  theme = theme_idea_light,

  title = tags$b("Site Suitability Analysis"),

  ## Sidebar -----------------------------------------------------------------

  sidebar = sidebar(

    accordion(

      accordion_panel(
        title = "Map view settings",

        tags$p(span("Zoom to location",
                    tooltip(bs_icon("info-circle"),
                            "Type in an address or select a region to zoom to a location",
                            placement="bottom"))),

        ### Region selector ------------------------------------------------------
        selectizeInput(
          "region_selector",
          label = "Select region",
          choices = regions$region,
          selected = "Austin"
        ),

        tags$hr(),

        ### Map type selector ----------------------------------------------------

        tags$p(span("Map type",
                    tooltip(bs_icon("info-circle"),
                            "Select a map type",
                            placement="bottom"))),

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
        ),

        tags$hr(),

        ### Screenshot map ----

        capture::capture(
          selector = "#map",
          filename = glue::glue("site-selection-map_{date}"),
          bs_icon("camera-fill"), tags$b("Screenshot map")
        )
      ),

      accordion_panel(
        title = "Layer settings",

        tags$p(span("Layer controls",
                    tooltip(bs_icon("info-circle"),
                            "Add layers to map; filters for display layers will appear below",
                            placement="bottom"))),

        ### SSI selector ---------------------------------------------------------
        input_switch(
          id = "ssi_switch",
          label = "Show site suitability index",
          value = TRUE
        ),

        ### Layer selector -------------------------------------------------------
        checkboxGroupInput(
          "layer_selector",
          label = "Select layer to display",
          choices = list(
            "Counties" = "counties",
            "Texas Accountability" = "accountability_tx",
            "Louisiana Accountability" = "accountability_la",
            "Florida Accountability" = "accountability_fl",
            "Ohio Accountability" = "accountability_oh",
            "IDEA Students" = "idea_stus",
            "Drive times" = "drive_times",
            "Children by age" = "children_by_age",
            "Households with children under 18" = "households_children_under18",
            "Children in Poverty" = "children_in_poverty",
            "Household Income" = "household_income",
            "Students in Poverty" = "students_in_poverty"
          ),
          selected = "counties"
        ),

        ### Filter inputs --------------------------------------------------------
        tags$hr(),
        tags$p(span("Layer Filters",
                    tooltip(bs_icon("info-circle"),
                            "Filters for displayed layers will appear below, as needed.",
                            placement = "bottom"))),

        #### school operation filter ----------------------------------------------------------
        # conditionalPanel(
        #   condition = "input.ssi_switch == true",
        #   checkboxGroupInput("school_operation_filter",
        #                      "School operation filter",
        #                      choices = c("In Operation",
        #                                  "Year 0",
        #                                  "Proposed",
        #                                  "Closed"),
        #                      selected = "In Operation")
        #
        #   )
        # ),

        #### SSI filter ----------------------------------------------------------
        conditionalPanel(
          condition = "input.ssi_switch == true",
          sliderInput("ssi_filter",
                      "SSI filter",
                      min = round(min(sf_site_suitability$index_total),2)-.01,
                      max = round(max(sf_site_suitability$index_total),2)+.01,
                      value = c(min(sf_site_suitability$index_total),
                                max(sf_site_suitability$index_total)
                      )

          )
        ),

        #### drive times filter --------------------------------------------------
        conditionalPanel(
          condition = "input.layer_selector.includes('drive_times')",
          sliderInput("time_filter",
                      "Drive times filter",
                      min = 1,
                      max = 15,
                      value = c(1, 10))
        ),

        #### IDEA schools filter ------------------------------------------------
        conditionalPanel(
          condition = "input.layer_selector.includes('idea_stus') ||
                         input.layer_selector.includes('drive_times')",
          uiOutput("schools_in_regions"),
          checkboxGroupInput("operations_filter",
                             "Operations filter",
                             choices = c("Proposed",
                                         "Year 0",
                                         "In Operation",
                                         "Closed"),
                             selected = c("Proposed",
                                          "Year 0",
                                          "In Operation",
                                          "Closed"))
        ),





        #### Children by age filter ----------------------------------------------------------
        conditionalPanel(
          condition = "input.layer_selector.includes('children_by_age')",
          checkboxGroupInput("children_by_age_filter",
                             "Children by age filter",
                             choices = c("Under 5 years",
                                         "5 to 9 years",
                                         "10 to 14 years",
                                         "15 to 17 years"),
                             selected = c("Under 5 years",
                                          "5 to 9 years",
                                          "10 to 14 years",
                                          "15 to 17 years")
          )
        ),

        #### Chirldren in poverty filter -------------------------------------------
        conditionalPanel(
          condition = "input.layer_selector.includes('children_in_poverty')",
          checkboxGroupInput("children_in_poverty_filter",
                             "Children in poverty filter",
                             choices = c("Under 1.00",
                                         "1.00 to 1.99"),
                             selected = c("Under 1.00",
                                          "1.00 to 1.99")
          )
        ),

        #### Household Income filter ----------------------------------------------
        conditionalPanel(
          condition = "input.layer_selector.includes('household_income')",
          checkboxGroupInput("household_income_filter",
                             "Household income filter",
                             choices = c("Less than $20,000",
                                         "$20,000 to $29,999",
                                         "$30,000 to $39,999",
                                         "$40,000 to $49,999",
                                         "$50,000 to $59,999",
                                         "$60,000 to $74,999",
                                         "$75,000 to $99,999",
                                         "$100,000 or more"),
                             selected = c("Less than $20,000",
                                          "$20,000 to $29,999",
                                          "$30,000 to $39,999",
                                          "$40,000 to $49,999",
                                          "$50,000 to $59,999",
                                          "$60,000 to $74,999",
                                          "$75,000 to $99,999",
                                          "$100,000 or more")
          )
        ),

        #### Students in poverty filter -------------------------------------------
        conditionalPanel(
          condition = "input.layer_selector.includes('students_in_poverty')",
          checkboxGroupInput("students_in_poverty_filter",
                             "Students in poverty filter",
                             choices = c("Enrolled in nursery school, preschool",
                                         "Enrolled in kindergarten",
                                         "Enrolled in grade 1 to grade 4",
                                         "Enrolled in grade 5 to grade 8",
                                         "Enrolled in grade 9 to grade 12"),
                             selected = c("Enrolled in nursery school, preschool",
                                          "Enrolled in kindergarten",
                                          "Enrolled in grade 1 to grade 4",
                                          "Enrolled in grade 5 to grade 8",
                                          "Enrolled in grade 9 to grade 12")
          )
        )



        #verbatimTextOutput("clicked_feature")
      ),

      #### Accordion panel: About the dashboard --------------------------------
      accordion_panel(
        title = "About the dashboard",

        # text
        tags$p(icon("rotate-right", lib = "font-awesome"),
               "App last updated ", tags$b("May 9, 2025."),
               tags$br(), tags$br(),
               "Census data from 5-Year American Community Survey, 2018-2023.",
               tags$br(), tags$br(),
               "IDEA schools last updated October 2024. IDEA students last updated May 2025.",
               tags$br(), tags$br(),
               "Florida, Lousisiana, and Ohio accountability data from 2024. Texas accountability data from 2023."),
        tags$p(icon("pen", lib = "font-awesome"),
               "Created by the ",
               tags$a(href = "https://ideapublicschoolsorg.sharepoint.com/RA/SitePages/Home.aspx", "Research & Analytics team."),
               "Please contact ",
               tags$a(href = "mailto:steven.macapagal@ideapublicschools.org?subject=Site Suitability App", "Steven Macapagal"),
               "or ",
               tags$a(href = "mailto:christopher.haid@ideapublicschools.org?subject=Site Suitability App", "Chris Haid"),
               "with any questions about the dashboard or any errors you find."),
        tags$p(icon("handshake", lib = "font-awesome"),
               "Partnership with the Growth team."),
        tags$p(icon("file", lib = "font-awesome"),
               "Version 2.2.0")

      )
    )
  ),
  ## Main body --------------------------------------------------------------

  ### Map -------------------------------------------------------------------
  nav_panel(
    title = "Suitability maps",

    card(
      title = "Map",full_screen = TRUE,
      mapboxglOutput("map", height = "400px")
    ),

    add_busy_bar(color = idea_colors_2024$lime, height = "8px")
  ),

  nav_panel(
    title = "How to use this app",

    navset_card_underline(

      nav_panel(
        title = "Common tasks"
      ),

      nav_panel(
        title = "Site suitability index",

        layout_column_wrap(
          width = 1/2,

          card(
            card_header("Building the index"),

            card_body(
              p("The site suitability index is composed of five variables:"),
              p("Number of children"),
              p("Number of students in poverty"),
              p("Number of low income households (< $50,000)"),
              p("Number of low-quality schools"),
              p("Number of high-quality schools"),
            )

          ),

          card(
            title = "Interpreting the index"
          )
        )
      )
    )

  ),

)

# Server ----------------------
server <- function(input, output) {


  ## Initial map --------------------------------------------------------------
  output$map <- renderMapboxgl({
    #req(input$region_selector)

    #     show_modal_progress_line(value = 0.05, "Loading map data and finding Austin...")
    initial_map <-
      mapboxgl(
        center = c(-97.661324953909, 30.3381543635444),
        zoom = 8.28974152189369,
        style = rdeck::mapbox_gallery_frank(),
        preserveDrawingBuffer = TRUE
      )

    ### add map controls ----
    initial_map <- initial_map %>%
      add_geocoder_control(
        position = "top-left",
        placeholder = "Search an address",
        collapsed = TRUE
      ) %>%
      add_navigation_control(
        position = "top-left",
        visualize_pitch = TRUE
      ) %>%
      add_reset_control(position = "top-left") %>%
      add_draw_control(
        position = "top-right",
        freehand = TRUE
      ) %>%
      add_scale_control(
        position = "bottom-right",
        unit = "imperial"
      )

    #      update_modal_progress(value = .1, "Loading site suitability data...",)
    initial_map <- initial_map %>%
      add_fill_layer(
        id = "suitability",
        source = sf_site_suitability,
        fill_color = interpolate(
          column = "index_total",
          values = c(.001, .01, .1, 1, 10, 100),
          stops  = c(idea_colors_2024$lightgray,
                     idea_colors_2024$gray,
                     idea_colors_2024$cyan,
                     idea_colors_2024$lime,
                     idea_colors_2024$melon,
                     idea_colors_2019$magenta),
          na_color = idea_colors_2024$coolgray),
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
        values = c(.001, .01, .1, 1, 10, 100),
        colors = c(idea_colors_2024$lightgray,
                   idea_colors_2024$gray,
                   idea_colors_2024$cyan,
                   idea_colors_2024$lime,
                   idea_colors_2024$melon,
                   idea_colors_2019$magenta),
      )

    #      update_modal_progress(value = .2, "Loading drive times data...")
    initial_map <- initial_map %>%
      add_fill_layer(
        id = "drive_time_isochrones",
        source = sf_isochrones_idea,
        fill_color = get_column("hex_code"),
        fill_opacity = 0.1,
        visibility = "none"
      )

    update_modal_progress(value = .3, "Loading schools...")
    initial_map <- initial_map %>%
      add_circle_layer(
        id ="schools",
        source = sf_idea_schools,
        slot = "top",
        circle_radius = 5,
        circle_color = get_column("hex_code"),
        circle_opacity = 1,
        tooltip = "SchoolShortName"
      ) %>%
      # add_layer(
      #   id = "school_districts",
      #   type = "fill",
      #   source = sf_school_districts_2022,
      #   paint = list(
      #     "line-color" = idea_colors_2024$blue,
      #     "line-width" = 1,
      #     "fill-opacity" = 0.1
      #   ),
      #   tooltip = "NAME"
      # ) %>%
      add_circle_layer(
        id = "accountability_tx",
        source = sf_accountability_schools_tx,
        circle_radius = 3.5,
        circle_color = match_expr(
          "overall_rating",
          values = c("A", "B", "C", "D", "F"),
          stops = idea_palette_ramp("qual")(5)
        ),
        circle_opacity = 1,
        tooltip = "overall_score",
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "accountability_la",
        source = sf_accountability_schools_la,
        circle_radius = 3.5,
        circle_color = match_expr(
          "letter_grade",
          values = c("A", "B", "C", "D", "F"),
          stops = idea_palette_ramp("qual")(5)
        ),
        circle_opacity = 1,
        tooltip = "sps",
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "accountability_fl",
        source = sf_accountability_schools_fl,
        circle_radius = 3.5,
        circle_color = match_expr(
          "grade_2024",
          values = c("A", "B", "C", "D", "F"),
          stops = idea_palette_ramp("qual")(5)
        ),
        circle_opacity = 1,
        tooltip = "pct_total_possible_points",
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "accountability_oh",
        source = sf_accountability_schools_oh,
        circle_radius = 3.5,
        circle_color = match_expr(
          "achievement_rating",
          values = c("5 Stars", "4 Stars", "3 Stars", "2 Stars", "1 Star"),
          stops = idea_palette_ramp("qual")(5)
        ),
        circle_opacity = 1,
        tooltip = "pi",
        visibility = "none"
      ) %>%
      add_layer(
        id = "counties",
        type = "line",
        source = sf_counties %>%
          filter(!is.na(region)),
        paint = list(
          "line-color" = idea_colors_2024$gray,
          "line-width" = 1
        )
      )



    #update_modal_progress(value = .7, "Getting layers from mapbox...")

    initial_map <- initial_map %>%
      add_vector_source(
        url = "mapbox://christopher-haid.tileset_ss_2025-05-09",
        id = "mapbox_layer"
      ) %>%
      add_circle_layer(
        id = "household_income",
        source = "mapbox_layer",
        source_layer = "acs_hh_income",
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
          stops = idea_palette_ramp("blueorange")(8)
        ),
        circle_opacity = .5,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "students_in_poverty",
        source = "mapbox_layer",
        source_layer = "acs_students_in_poverty",
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("Enrolled in nursery school, preschool",
                     "Enrolled in kindergarten",
                     "Enrolled in grade 1 to grade 4",
                     "Enrolled in grade 5 to grade 8",
                     "Enrolled in grade 9 to grade 12"),
          stops = students_in_poverty_palette[1:5]
        ),
        circle_opacity = .333,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "households_children_under18",
        source = "mapbox_layer",
        source_layer = "acs_hh_children_under_18",
        circle_radius = 2,
        circle_color = "#626363",
        circle_opacity = .333,
        visibility = "none"
      ) %>%
      add_circle_layer(
        id = "children_in_poverty",
        source = "mapbox_layer",
        source_layer = "acs_children_in_poverty",
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("Under 1.00",
                     "1.00 to 1.99"),
          stops = children_in_poverty_palette
        ),
        circle_opacity = .5,
        visibility = "none"
      ) %>%

      add_circle_layer(
        id = "children_by_age",
        source = "mapbox_layer",
        source_layer = "acs_children_by_age",
        circle_radius = 2,
        circle_color = match_expr(
          column = "variable",
          values = c("Under 5 years",
                     "5 to 9 years",
                     "10 to 14 years",
                     "15 to 17 years"),
          stops = children_by_age_block_palette
        ),
        circle_opacity = .45,
        visibility = "none"
      ) %>%

      add_circle_layer(
        id = "students-layer",
        source = "mapbox_layer",
        source_layer = "idea_students_24_25",
        circle_radius = 2.5,
        circle_color = get_column("hex_code"),
        circle_opacity = .65,
        visibility = "none"
      )

    #remove_modal_progress()

    initial_map


  })

  ## search ----

  observeEvent(input$map_geocoder, {

    result <- input$map_geocoder$result

    output$result <- renderPrint(result)

  })

  ## search bar observer ----
  # observe({
  #
  #   sf_geocoded_address <- mapboxapi::geocoder_as_sf(input$search_box) %>%
  #     mutate(id = "search_marker",
  #            address = input$search_box[7],
  #            long = as.vector(input$search_box[[10]])[1],
  #            lat = as.vector(input$search_box[[10]])[2])
  #
  #   mapboxgl_proxy("map") %>%
  #     add_markers(
  #       sf_geocoded_address,
  #       color = idea_colors_2024$vermillion,
  #       marker_id = "id",
  #       popup = "address", # \n{coords}"),
  #       draggable = TRUE
  #     ) %>%
  #     fly_to(
  #       center = as.vector(c(sf_geocoded_address$long, sf_geocoded_address$lat)),
  #       zoom = 12
  #     )
  #
  # }) %>%
  #   bindEvent(input$search_box, ignoreNULL = TRUE)

  ## ssi layer observer ------------------------------------------------------
  observeEvent(input$ssi_switch, {
    #req(input$ssi_switch)
    if (input$ssi_switch == FALSE) {

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
  observeEvent(input$region_selector,{

    req(input$region_selector)
    mapboxgl_proxy("map") %>%
      fit_bounds(
        sf_counties %>%
          filter(region == input$region_selector),
        animate = TRUE
      )

  })

  #map type observer ---------------------------------------------------------
  observeEvent(input$map_type_selector, {
    #req(input$region_selector)
    if (input$map_type_selector != "frankfurt") {

      mapboxgl_proxy("map")  %>%
        #set_view(input$map_center, input$map_zoom) %>%
        set_style(mapbox_style(input$map_type_selector), diff = FALSE) # %>%
      # fit_bounds(
      #   sf_site_suitability %>%
      #     filter(REGION == input$region_selector),
      #   animate = FALSE
      # )

    } else {

      mapboxgl_proxy("map") %>%
        set_style(style = "mapbox://styles/mapbox-map-design/ckshxkppe0gge18nz20i0nrwq", diff = FALSE) %>%
        set_view(input$map_center, input$map_zoom)

    }

  })

  ## layer selector observer -------------------------------------------------
  observeEvent(input$layer_selector, {
    ### Counties ----------------------------------------------------------
    if ("counties" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("counties", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("counties", "visibility",  "none")

    }

    ### Accountability TX ----------------------------------------------------------
    if ("accountability_tx" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_tx", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_tx", "visibility",  "none")

    }

    ### Accountability LA ----------------------------------------------------------
    if ("accountability_la" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_la", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_la", "visibility",  "none")

    }

    ### Accountability FL ----------------------------------------------------------
    if ("accountability_fl" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_fl", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_fl", "visibility",  "none")

    }

    ### Accountability OH ----------------------------------------------------------
    if ("accountability_oh" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_oh", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("accountability_oh", "visibility",  "none")

    }

    ### IDEA Students --------------------------------------------------------
    if ("idea_stus" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("students-layer", "visibility",  "visible")

    } else {
      #if({"idea_stus" %in% input$layer_selector}){
      mapboxgl_proxy("map") %>%
        set_layout_property("students-layer", "visibility", "none")

    }

    ### Drive times ----------------------------------------------------------
    if ("drive_times" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("drive_time_isochrones", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("drive_time_isochrones", "visibility",  "none")

    }

    ### Children by age -----------------------------------------------------
    if ("children_by_age" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("children_by_age", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("children_by_age", "visibility",  "none")

    }

    ### Children in poverty ---------------------------------------------------
    if ("children_in_poverty" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("children_in_poverty", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("children_in_poverty", "visibility",  "none")

    }

    ### Households children under 18 -----------------------------------------
    if ("households_children_under18" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("households_children_under18", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("households_children_under18", "visibility",  "none")

    }

    ### Students in poverty ---------------------------------------------------
    if ("students_in_poverty" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("students_in_poverty", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("students_in_poverty", "visibility",  "none")

    }

    ### Household income ---------------------------------------------------
    if ("household_income" %in% input$layer_selector) {

      mapboxgl_proxy("map") %>%
        set_layout_property("household_income", "visibility", "visible")

    } else {

      mapboxgl_proxy("map") %>%
        set_layout_property("household_income", "visibility",  "none")

    }

  },
  ignoreNULL = FALSE
  )

  ## Filter observers ---------------------------------------------------------
  ### SSI filter -------------------------------------------------------------
  observeEvent(input$ssi_filter, {

    req(input$ssi_filter)
    mapboxgl_proxy("map") %>%
      set_filter(
        "suitability",
        list(
          "all",
          list(">=", get_column("index_total"), input$ssi_filter[1]),
          list("<=", get_column("index_total"), input$ssi_filter[2])
        )
      )
  })

  ### drive time filter -------------------------------------------------------------
  # toListen <- reactive({
  #   list(input$time_filter,
  #        input$schools_selector)
  # })

  observeEvent(input$time_filter, {

    req(input$time_filter)
    # req(input$schools_selector)
    mapboxgl_proxy("map") %>%
      # set_filter("drive_time_isochrones",
      #            c("in", "SchoolShortName", input$schools_selector)) %>%
      set_filter("drive_time_isochrones",
                 list("all",
                      list(">=", get_column("time"), input$time_filter[1]),
                      list("<=", get_column("time"), input$time_filter[2]))

      )
  })

  ### drive time filter -------------------------------------------------------------
  # observeEvent(input$time_filter, {
  #
  #   req(input$time_filter)
  #   mapboxgl_proxy("map") %>%
  #     set_filter("drive_time_isochrones",
  #                c("in", "SchoolShortName", input$schools_selector))
  #
  # })

  # observeEvent(input$drive_time_controls, {
  #
  #   req(input$drive_time_controls)
  #   mapboxgl_proxy("map") %>%
  #     set_filter(
  #       "drive_time_isochrones",
  #       list("in", "time", input$drive_time_controls)
  #     )
  #
  # })

  ### IDEA schools filters -------------------------------------------------
  observeEvent(input$schools_selector, {

    req(input$schools_selector)
    mapboxgl_proxy("map") %>%
      set_filter(
        "students-layer",
        c("in", "school_short_name", input$schools_selector)
      )

  })


  ### operations filters -------------------------------------------------
  observeEvent(input$operations_filter, {

    req(input$operations_filter)
    mapboxgl_proxy("map") %>%
      set_filter(
        "schools",
        c("in", "SiteStatus", input$operations_filter)
      )

  })


  ### Children by age filter ------------------------------------------------
  observeEvent(input$children_by_age_filter, {

    req(input$children_by_age_filter)
    mapboxgl_proxy("map") %>%
      set_filter(
        "children_by_age",
        c("in", "variable", input$children_by_age_filter)
      )

  })

  ### Children in poverty filter ------------------------------------------------
  observeEvent(input$children_in_poverty_filter, {

    req(input$children_in_poverty_filter)
    mapboxgl_proxy("map") %>%
      set_filter(
        "children_in_poverty",
        c("in", "variable", input$children_in_poverty_filter)
      )

  })

  ### Household income filter ------------------------------------------------
  observeEvent(input$household_income_filter, {

    req(input$household_income_filter)
    mapboxgl_proxy("map") %>%
      set_filter(
        "household_income",
        c("in", "variable", input$household_income_filter)
      )

  })

  ### Students in poverty filter ------------------------------------------------
  observeEvent(input$students_in_poverty_filter, {

    req(input$students_in_poverty_filter)
    mapboxgl_proxy("map") %>%
      set_filter(
        "students_in_poverty",
        c("in", "variable", input$students_in_poverty_filter)
      )

  })

  ## UI RENDER ---------------------------------------------------------------
  ### IDEA schools input controls ---------------------------------------------
  output$schools_in_regions <- renderUI({

    req(input$region_selector)
    schools <- sf_idea_schools %>%
      filter(Region == input$region_selector) %>%
      pull(SchoolShortName)
    checkboxGroupInput(
      "schools_selector",
      label = "Select schools",
      choices = schools,
      selected = schools
    )

  })

  ### drive time input controls ---------------------------------------------
  # output$time_filter <- renderUI({
  #
  #   req(input$region_selector)
  #   times <- 1:15
  #   schools <- sf_idea_schools %>%
  #     filter(Region == input$region_selector) %>%
  #     pull(SchoolShortName)
  #   isochrones <- sf_isochrones_idea %>%
  #     filter(Region == input$region_selector)
  #   checkboxGroupInput(
  #     "time_selector",
  #     label = "Select drive times",
  #     choices = times,
  #     selected = times
  #   )
  #
  # })



}
# Run the application
shinyApp(ui = ui, server = server)
