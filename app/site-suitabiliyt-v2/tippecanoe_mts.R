library(mapboxapi)
library(mapgl)
library(sfarrow)
library(tidyverse)


# Data -------------------------------------------------------------------------------------
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




GET("https://usbuildingdata.blob.core.windows.net/usbuildings-v1-1/Texas.zip",
    write_disk("Texas.zip", overwrite = TRUE), progress())

unzip("Texas.zip")

# tippecanoe time -----
tippecanoe(input = "Texas.geojson",
           output = "Texas.mbtiles",
           layer_name = "texas_buildings")

upload_tiles(input = "Texas.mbtiles",
             username = "christopher-haid",
             tileset_id = "TX_buildings",
             multipart = TRUE,
             access_token = 'sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw')


mapdeck(token = 'sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw',
        style = "mapbox://styles/christopher-haid/clzswus269zxi1nnul5nr7y1u/draft",
        zoom = 6,
        location = c(-98.7382803, 31.7678448))


mapboxapi::mts_list_tilesets("christopher-haid",
                             access_token = 'sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw')

mapboxgl(center = c(-97.661324953909, 30.3381543635444),
         zoom = 8.28974152189369,
         #style = "mapbox://styles/christopher-haid/clzty4w5000pw01pc131d02nd/draft",
         #access_token = 'sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw'
         ) %>%
  add_vector_source(url = "mapbox://christopher-haid.TX_buildings",
                          #"https://api.mapbox.com/styles/v1/christopher-haid/clzty4w5000pw01pc131d02nd/draft.html?title=view",
                          #"https://api.mapbox.com/styles/v1/christopher-haid/clzty4w5000pw01pc131d02nd/wmts?access_token=pk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbDNqNmJuN28wMDVkM2pvNmg5bWI3bHpqIn0.eQlM_puWEM7v7QfkflYA1A/draft",
                    id = "texas") %>%
  add_fill_layer(id = "texas-buildings",
                 source = "texas",
                 source_layer = "texas_buildings",
                 fill_color = "#ff00ef",
                 fill_opacity = 0.5
                 ) %>%
  add_layers_control()

##### Tippecanoe with data loaded in app.R --------------


tippecanoe(input = sf_household_income %>% filter(variable != "Total"),
           output = "sf_household_income.mbtiles",
           layer_name = "household_income", overwrite = TRUE)


upload_tiles(input = "sf_household_income.mbtiles",
             username = "christopher-haid",
             tileset_id = "household_income",
             multipart = TRUE,
             access_token = 'sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw')


initial_map <- mapboxgl(center = c(-97.661324953909, 30.3381543635444),
                        zoom = 8.28974152189369,
                        style = rdeck::mapbox_gallery_frank())


initial_map %>%
  add_vector_source(url = "mapbox://christopher-haid.household_income",
                    #"https://api.mapbox.com/styles/v1/christopher-haid/clzty4w5000pw01pc131d02nd/draft.html?title=view",
                    #"https://api.mapbox.com/styles/v1/christopher-haid/clzty4w5000pw01pc131d02nd/wmts?access_token=pk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbDNqNmJuN28wMDVkM2pvNmg5bWI3bHpqIn0.eQlM_puWEM7v7QfkflYA1A/draft",
                    id = "hhi") %>%
  add_circle_layer(
    id = "household_income",
    source = "hhi",
    source_layer = "household_income",
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
    circle_opacity = .5,
    visibility = "none"
  ) %>%
  add_layers_control()



# MTS version of the above tileset creation ----

# Create source
hhi_source <- mts_create_source(data = sf_household_income %>% filter(variable != "Total"),
                                tileset_id = "household_income",
                                username = "christopher-haid",
                                access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
                                )

test_hhi_recipe <- mts_make_recipe(
  hhi = recipe_layer(
    source = hhi_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  )
)

mts_validate_recipe(test_hhi_recipe)

mts_create_tileset(
  tileset_name = "test_hhi_tileset",
  username = "christopher-haid",
  recipe = test_hhi_recipe,
  access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)

mts_update_recipe(
  tileset_name = "test_hhi_tileset",
  username = "christopher-haid",
  recipe = test_hhi_recipe,
  access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)

mts_publish_tileset(tileset_name = "test_hhi_tileset",
                    username = "christopher-haid",
                    access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
                    )

initial_map %>%
  add_vector_source(url = "mapbox://christopher-haid.test_hhi_tileset",
                    #"https://api.mapbox.com/styles/v1/christopher-haid/clzty4w5000pw01pc131d02nd/draft.html?title=view",
                    #"https://api.mapbox.com/styles/v1/christopher-haid/clzty4w5000pw01pc131d02nd/wmts?access_token=pk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbDNqNmJuN28wMDVkM2pvNmg5bWI3bHpqIn0.eQlM_puWEM7v7QfkflYA1A/draft",
                    id = "hhi") %>%
  add_circle_layer(
    id = "household_income",
    source = "hhi",
    source_layer = "hhi",
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
    circle_opacity = .5,
    visibility = "none"
  ) %>%
  add_layers_control()

## MTS Multi-layer tile test -----------------------------

hhi_source <- mts_create_source(data = sf_household_income %>% filter(variable != "Total"),
                                tileset_id = "household_income",
                                username = "christopher-haid",
                                access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)




idea_students_source <- mts_create_source(data = sf_idea_students_mvp,
                                    tileset_id = "idea_students",
                                    username = "christopher-haid",
                                    access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)



children_by_age_source <- mts_create_source(data = sf_children_by_age_block_group,
                                          tileset_id = "children_by_age",
                                          username = "christopher-haid",
                                          access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)


children_in_poverty_source <- mts_create_source(data = sf_children_in_poverty,
                                            tileset_id = "children_in_poverty",
                                            username = "christopher-haid",
                                            access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)


under18_source <- mts_create_source(data = sf_households_under18,
                                    tileset_id = "under18",
                                    username = "christopher-haid",
                                    access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)


students_in_poverty_source <- mts_create_source(data = sf_students_in_poverty,
                                    tileset_id = "students_in_poverty",
                                    username = "christopher-haid",
                                    access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)




test_hhi_recipe <- mts_make_recipe(
  hhi = recipe_layer(
    source = hhi_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  ),

  idea_students = recipe_layer(
    source = idea_students_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  ),

  children_by_age = recipe_layer(
    source = children_by_age_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  ),

  children_in_poverty = recipe_layer(
    source = children_in_poverty_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  ),

  students_in_poverty = recipe_layer(
    source = students_in_poverty_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  ),

  under18 = recipe_layer(
    source = under18_source$id,
    minzoom = 3,
    maxzoom = 14,
    tiles = tile_options(layer_size = 2500),
    features = feature_options(simplification = 10)
  )
)

mts_validate_recipe(test_hhi_recipe)



mts_update_recipe(
  tileset_name = "test_hhi_tileset",
  username = "christopher-haid",
  recipe = test_hhi_recipe,
  access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)

mts_publish_tileset(tileset_name = "test_hhi_tileset",
                    username = "christopher-haid",
                    access_token = "sk.eyJ1IjoiY2hyaXN0b3BoZXItaGFpZCIsImEiOiJjbHpzd3RzcjcyMnh3Mmtwd3BsbW13NmprIn0.-loLhHhe75ifjWB3_9Yovw"
)

