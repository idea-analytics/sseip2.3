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

# Data -----------------------------------------------------------------------
sf_site_suitability_mvp<-sfarrow::st_read_feather("sf_site_suitability.feather") %>%
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





# UI ------------------------------------------------------------------------
ui <- page_sidebar(

    title = "Site Suitability Analysis",
    sidebar = sidebar(
      title = "Layer controls",

      ## Region selector ------------------------------------------------------
      selectizeInput(
        "region_selector",
        label = "Select region",
        choices = regions$REGION
      ),

      ## SSI selector ---------------------------------------------------------
      input_switch(
        id = "ssi_switch",
        label = "Show site suitability index",
        value = TRUE

        ),
      ## Layer selector -------------------------------------------------------

      selectizeInput(
         "layer_selector",
        label = "Select layer to display",
        choices = list(`IDEA`        = list("IDEA Students" = "idea_stus",
                                           "Drive times" = "drive_times"),
                      `Population` =list("Children by age" = "children_by_age",
                                          "Households with children under 18" = "households_with_children_under_18"),
                      `Poverty`    =   list("Children in Poverty" = "children_in_poverty",
                                          "Household Income" = "household_income",
                                          "Students in Poverty" = "students_in_poverty"))
        ),
      ),

    card(
      title = "Map",
      mapboxglOutput("map")
      )

)

# Server ----------------------
server <- function(input, output) {

  ## Intial map --------------------------------------------------------------
  output$map <- renderMapboxgl({

    mapboxgl(style = mapbox_gallery_frank()) %>%
      fit_bounds(sf_site_suitability_mvp %>% filter(REGION=="Austin"), animate = T) %>%
      add_fill_layer(id = "suitability",
                     source = sf_site_suitability_mvp_2,
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
        values = c(.001, .01, .1, 1),
        colors  = c(idea_colors$lightgray,
                    idea_colors$gray,
                    idea_colors$melon,
                    idea_colors$magenta)
      )

  })



  observeEvent(input$region_selector, {
    mapboxgl_proxy("map") %>%
      fit_bounds(sf_site_suitability_mvp %>% filter(REGION==input$region_selector), animate = T)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
