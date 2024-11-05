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



# Data -----------------------------------------------------------------------

df_counties_tx <- tibble::tribble(
  ~ state, ~ region, ~ county,

  "TX", "Austin", "Travis County",
  "TX", "Austin", "Hays County",
  "TX", "Austin", "Bastrop County",
  "TX", "Austin", "Williamson County",

  "TX", "Coastal Bend", "Nueces County",

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
  "TX", "Tarrant County", "Dallas County"
)

df_counties_ips <- tibble::tribble(
  ~ state, ~ region, ~ county,

  "OH", "Cincinnati", "Hamilton County",

  "FL", "Jacksonville", "Duval County",
  "FL", "Tampa", "Hillsborough County",
  "FL", "Tampa", "Polk County",
  "FL", "Orlando", "Orange County",

  "LA", "Southern Louisiana", "East Baton Rouge Parish",
  "LA", "Southern Louisiana", "Orleans Parish"
)

df_counties_all <- dplyr::bind_rows(
  df_counties_tx,
  df_counties_ips
)


sf_site_suitability_mvp <- st_read_feather("sf_site_suitability.feather") %>%
  filter(!is.na(h3_address)) %>%
  mutate(popup = glue::glue("<strong>Site Suitability Index: {round(index_total,2)}</strong>",
                            "Students in poverty index: {round(index_students_poverty,2)}",
                            "Low quality schools count: {round(total_low_quality_nn_schools,2)}",
                            "High quality schools count: {round(total_high_quality_nn_schools,2)}",
                            .sep = "<br>"),
         tooltip = glue::glue("SSI {round(index_total,2)}")
  )

# regions <- sf_site_suitability_mvp %>%
#   as_tibble() %>%
#   dplyr::select(REGION) %>%
#   dplyr::distinct() %>%
#   arrange(REGION)




#sf_dot_density_mvp <- st_read_feather("sf_dot_density_mvp.feather")

hex_codes <- read.csv("school_hex_codes.csv")
sf_isochrones_idea_mvp <- st_read_feather("sf_isochrones_idea_2024.feather") %>%
  inner_join(hex_codes, by = c("SchoolShortName" = "school_short_name"))

sf_idea_schools_mvp <- st_read_feather("sf_schools_idea_2024.feather") %>%
  inner_join(hex_codes, by = c("SchoolShortName" = "school_short_name"))

sf_states <- st_read_feather("sf_states_2022.feather")

sf_counties <- st_read_feather("sf_counties_2022.feather") %>%
  left_join(sf_states %>%
              as.data.frame() %>%
              select(STATEFP,
                     STUSPS,
                     STATENAME = NAME),
            by = c("STATEFP" = "STATEFP")) %>%
  left_join(df_counties_all,
            by = c("NAMELSAD" = "county",
                   "STUSPS" = "state"))

sf_school_districts_2022 <- st_read_feather("sf_school_districts_2022.feather") %>%
  left_join(sf_states %>%
              as.data.frame() %>%
              select(STATEFP,
                     STUSPS,
                     STATENAME = NAME),
            by = c("STATEFP" = "STATEFP"))

regions <- sf_counties %>%
  as_tibble() %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::select(region) %>%
  dplyr::distinct() %>%
  arrange(region)


theme_idea_light <- bs_theme(bootswatch = "yeti") %>%
  bs_theme_update(primary = "#002F6C")



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

        tags$br(),
        mapboxapi::mapboxGeocoderInput("search_box",
                                       placeholder = "Search for an address"),

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
        )
      ),



      #### Accordion panel: About the dashboard --------------------------------
      accordion_panel(
        title = "About the dashboard",

        # text
        tags$p(icon("rotate-right", lib = "font-awesome"),
               "Last updated ", tags$b("October 21, 2024")),
        tags$p(icon("pen", lib = "font-awesome"),
               "Created by the ",
               tags$a(href = "https://ideapublicschoolsorg.sharepoint.com/RA/SitePages/Home.aspx", "Research & Analytics team"),
               ". Please contact ",
               tags$a(href = "mailto:steven.macapagal@ideapublicschools.org?subject=Site Suitability App", "Steven Macapagal"),
               "or ",
               tags$a(href = "mailto:christopher.haid@ideapublicschools.org?subject=Site Suitability App", "Chris Haid"),
               "with any questions about the dashboard or any errors you find."),
        tags$p(icon("handshake", lib = "font-awesome"),
               "Partnership with the Growth team."),
        tags$p(icon("file", lib = "font-awesome"),
               "Version 1.0.0.9000")

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
    title = "How to use this app"
  )
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
        style = rdeck::mapbox_gallery_frank()
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

    initial_map

  })


  ## search bar observer ----
  observe({

    sf_geocoded_address <- mapboxapi::geocoder_as_sf(input$search_box) %>%
      mutate(id = "search_marker",
             address = input$search_box[7],
             long = as.vector(input$search_box[[10]])[1],
             lat = as.vector(input$search_box[[10]])[2])

    mapboxgl_proxy("map") %>%
      add_markers(
        sf_geocoded_address,
        color = idea_colors_2024$vermillion,
        marker_id = "id",
        popup = "address", # \n{coords}"),
        draggable = TRUE
      ) %>%
      fly_to(
        center = as.vector(c(sf_geocoded_address$long, sf_geocoded_address$lat)),
        zoom = 12
      )

  }) %>%
    bindEvent(input$search_box, ignoreNULL = TRUE)


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
        set_style(mapbox_style(input$map_type_selector), diff = FALSE) %>%
      fit_bounds(
        sf_site_suitability_mvp %>%
          filter(REGION == input$region_selector),
        animate = FALSE
      )

    } else {

      mapboxgl_proxy("map") %>%
        set_style(style = "mapbox://styles/mapbox-map-design/ckshxkppe0gge18nz20i0nrwq", diff = FALSE) %>%
        set_view(input$map_center, input$map_zoom)

    }

  })


  ## UI RENDER ---------------------------------------------------------------
  ### IDEA schools input controls ---------------------------------------------
  output$schools_in_regions <- renderUI({

    req(input$region_selector)
    schools <- sf_idea_schools_mvp %>%
      filter(Region   == input$region_selector,
             SiteStatus == "In Operation") %>%
      pull(SchoolShortName)
    checkboxGroupInput(
      "schools_selector",
      label = "Select schools",
      choices = schools,
      selected = schools
    )

  })

}

# Run the application
shinyApp(ui = ui, server = server)
