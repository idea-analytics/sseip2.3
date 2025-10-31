# install.packages("mapboxapi") # if needed
library(mapboxapi)
library(sf)
library(purrr)
library(dplyr)
library(DBI)
library(scales)      # rescale
library(data.table)

# --- 0) Auth & who/what -----------------------------------------------
# Use a **secret** token with tilesets:write scope
Sys.getenv("MAPBOX_API_SECRET_TOKEN")

username    <- "christopher-haid"
tileset_name <- "ssi-layers"  # final tileset id: mapbox://christopher-haid.ssi2-layers


# #winsorize to 1st–99th percentile----
winsor <- function(x, p = c(0.01, 0.99), reverse=FALSE) {
  #if(reverse) x <- -x
  qs <- quantile(x, p, na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

# quantile/CDF stretch
cdf_stretch <- function(x, reverse = FALSE) {



  r <- rank(x, ties.method = "average", na.last = "keep")
  out <- (r - 1) / (sum(!is.na(x)) - 1)

  if(reverse) out <- 1 - out

  out
}

# gamma correction on [0,1]
gamma_stretch <- function(v01, gamma = 0.6) {
  v01 ^ gamma
}

# logistic around threshold t (0–1 scale)
logistic_stretch <- function(v01, t = 0.8, k = 12) {
  1 / (1 + exp(-k * (v01 - t)))
}


source("lib/st_joins_functions.R")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "INSTALL 'motherduck'")
dbExecute(con, "LOAD 'motherduck'")
dbExecute(con, "ATTACH 'md:'")
dbExecute(con, "USE md_test")
dbExecute(con, "INSTALL spatial")
dbExecute(con, "LOAD spatial")


normalize <- \(x) (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

md_get_for_map <- function(.con = con, .tbl, h3_tbl = h3_sf, pop_format = "default", reverse = FALSE){

  names_lookup <-  c(pred_estimate = "pred_est",
                     pred_variance = "pred_se")

  out <- tbl(con, .tbl) %>%
    inner_join(h3_tbl, by = "h3_address") %>%
    rename(h3id = h3_address) %>%
    rename(any_of(names_lookup))

  if(pop_format == "currency") {
    out<- out %>%
      mutate(popup =paste0("$",round(pred_estimate,0)),
             tooltip = popup) %>%
      md_collect_sf()
  }

  if(pop_format == "percent") {
    out<- out %>%
      mutate(popup =paste0(round(pred_estimate*100,1),"%"),
             tooltip = popup) %>%
      md_collect_sf()
  }

  if(pop_format == "default") {
    out<- out %>%
      mutate(popup = pred_estimate,
             tooltip = popup) %>%
      md_collect_sf()
  }

  out %>%
    mutate(normed_pred_estimate_1 = normalize(winsor(pred_estimate)),
           normed_pred_estimate = cdf_stretch(normed_pred_estimate_1, reverse))

}



# Kriged: 3rd reading ----
h3_sf <- tbl(con, "sf_h3")
#
assess_r7_3r <- md_get_for_map(.tbl = "kriged_sf_state_assessments_res7_pct_meets_reading_3",
                               pop_format = "percent",
                               reverse = TRUE)
#
# # Kriged: 7th math ----
#
assess_r7_7m <- md_get_for_map(.tbl = "kriged_sf_state_assessments_res7_pct_meets_math_7",
                                pop_format = "percent",
                                reverse = TRUE)



# Kriged: HHI ----
h3_sf <- tbl(con, "sf_h3")
#
hhi_r7_tx <- md_get_for_map(.tbl = "kriged_h3_res_7_TX_B19013_001_from_block_group",
                               pop_format = "currency",
                               reverse = TRUE)
#
# # Kriged: change in childe density ----
#
change_child_density_r7 <- md_get_for_map(.tbl = "kriged_sf_child_density_change_2018_2023_res7_log_shift",
                               #pop_format = "percent",
                               reverse = FALSE)

# # Kriged: students in poverty ----
#
students_in_poverty_r7 <- md_get_for_map(.tbl = "kriged_h3_res_7_TX_B14006_001_from_tract",

                               reverse = FALSE)



# --- 1) Register the layers you want to include ------------------------
# Put your sf objects here (names become recipe layer names)
layers <- list(
  hhi_r7_tx              = hhi_r7_tx,
  chng_child_dnsty_r7= change_child_density_r7,
  stus_in_poverty_r7 = students_in_poverty_r7,
  assess_r7_7m           = assess_r7_7m,
  assess_r7_3r           = assess_r7_3r
)

# Per-layer options (min/max zoom, tile options). Tweak as needed.
# H3 r7 ~ neighborhood scale; good interactive range ~5–12.
minzoom_default <- 7
maxzoom_default <- 12
layer_size_default <- 2500  # bigger tiles to reduce feature thinning at mid zooms

# --- 2) Create (or append to) MTS sources for each sf layer -----------
# Each source gets an ID like: mapbox://tileset-source/<username>/<src_id>
# Note: mts_create_source appends if the source id already exists (MTS behavior).
# If you want a clean overwrite, delete the source in Studio or via the API first.

create_source_safe <- function(x, nm) {
  src_id <- paste0(tileset_name, "_", nm)  # stable id per layer
  res <- mts_create_source(
    data = x,
    tileset_id = src_id,
    username = username,
    access_token = Sys.getenv("MAPBOX_API_SECRET_TOKEN")
  )
  list(name = nm, source_id = res$id)
}

sources <- purrr::imap(layers, create_source_safe)

# --- 3) Build an MTS recipe with one recipe_layer per source ----------
# Programmatically create named recipe layers, then pass them to mts_make_recipe()
make_recipe_layer <- function(src) {
  recipe_layer(
    source  = src$source_id,
    minzoom = minzoom_default,
    maxzoom = maxzoom_default,
    tiles   = tile_options(layer_size = layer_size_default, )
  )
}

recipe_layers <- setNames(lapply(sources, make_recipe_layer),
                          vapply(sources, `[[`, "", "name"))

recipe <- do.call(mts_make_recipe, recipe_layers)

# Validate before sending
stopifnot(mts_validate_recipe(recipe, access_token = Sys.getenv("MAPBOX_API_SECRET_TOKEN")))

# --- 4) Create or update the tileset, then publish --------------------
# Helper: does tileset already exist?
tilesets <- mts_list_tilesets(username, access_token = Sys.getenv("MAPBOX_API_SECRET_TOKEN"))
exists <- any(tilesets %>% pull(id) == paste0(username, ".", tileset_name))

if (!exists) {
  message("Creating tileset: ", tileset_name)
  mts_create_tileset(
    tileset_name = tileset_name,
    username     = username,
    recipe       = recipe,
    access_token =  Sys.getenv("MAPBOX_API_SECRET_TOKEN")
  )
} else {
  message("Tileset exists; updating recipe: ", tileset_name)
  mts_update_recipe(
    tileset_name = tileset_name,
    username     = username,
    recipe       = recipe,
    access_token =  Sys.getenv("MAPBOX_API_SECRET_TOKEN")
  )
}

# Publish (populate) tileset
mts_publish_tileset(
  tileset_name = tileset_name,
  username     = username,
  access_token =  Sys.getenv("MAPBOX_API_SECRET_TOKEN")
)

cat("\nDone.\nTileset URL: ", sprintf("mapbox://%s.%s", username, tileset_name), "\n")

