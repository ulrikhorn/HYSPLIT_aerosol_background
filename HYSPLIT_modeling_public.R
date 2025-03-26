## Hysplit function based 
library(splitr)
library(lubridate)
library(here)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(gridExtra)
library(htmltools)
library(googleway)
library(magrittr)
library(zoo)
library(raster)
library(terrainr)

lat = 59.9703617
lon = 11.0320524
days = c("2024-08-28")
daily_hours = c(0, 6, 12, 18)
duration = 24

get_trajectory <- function(lat = 59.9703617, lon = 11.0320524, duration = 24, days = c("2024-08-28"), daily_hours = c(0, 6, 12, 18)){
  
  trajectory <- 
    hysplit_trajectory(
      lat = lat,
      lon = lon,
      height = 10,
      duration = duration,
      days = days,
      daily_hours = daily_hours,
      direction = "backward",
      #    met_type = "gdas1",
      extended_met = TRUE,
      exec_dir = here::here("out")) 
  
  trajectory
}

get_elevation <- function(df, duration) {
  t <- data.frame(lat = df$lat, lon = df$lon)
  api_key = "get_your_own_key"
  elevation_t <- NULL
  if (nrow(t) > 100) {
    
    for (i in seq(1, nrow(t), by = 100)) {
      start_row <- i
      end_row <- min(i + 99, nrow(t)) # Ensure you don't go beyond the last row
      current_rows <- df[start_row:end_row, ]
      elevation_t <- bind_rows(elevation_t, google_elevation(df_locations = current_rows, key = api_key)$results)
    }
  }else{
    elevation_t <- google_elevation(df_locations = t, key = api_key)$results
  }
  
  #fram <- 
  elevation <- elevation_t
  #elevation <- elevation_t[["results"]]
  elevation$time <-
    rep(seq(from = 0, to = -24), times = nrow(df) / 25)
  
  df$altitude <- elevation$elevation
  
  df <-
    df %>% mutate(over_land = ifelse(altitude > 0, 'yes', 'no'))
  
  df <-
    df %>% mutate(true_height = ifelse(altitude < 0, height, altitude + height))
  df <-
    df %>% mutate(date = str_extract(traj_dt_i, pattern = "-[0-9][0-9]-[0-9][0-9]") %>% str_replace('-', ''))
  df
  
}

overland_stats <- function(df_long) {
  percent_overland <-
    df_long %>% group_by(traj_dt_i) %>% count(over_land) %>% pivot_wider(values_from = n, names_from = over_land) %>% mutate(percent = yes /
                                                                                                                               25)
  
  mean_elevation <-
    df_long %>% dplyr::select(c(8, true_height, over_land)) %>% group_by(traj_dt_i) %>% summarise(mean_value = mean(true_height))
  
  overland_elevation <-
    left_join(mean_elevation, percent_overland) %>% dplyr::select(c(1, 2, 5))
  
  overland_elevation$traj_dt_i <-
    as.POSIXct(overland_elevation$traj_dt_i, format = "%Y-%m-%d %H:%M:%S")
  
  overland_elevation
}

make_raster_extract_landcover <- function(rasters, df_long){
  
  tif_vector <- # map tiles
    c(
      'E000N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif',
      'E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif',
      'E020N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif'
    )
  
  
  merged_rasters <- merge_rasters(input_rasters = tif_vector,
                                  output_raster = tempfile(fileext = "merged.tif"))
  
  clc <- raster(merged_rasters)
  
  long_lats <- df_long %>% dplyr::select(c(lon, lat))
  
  points <- SpatialPoints(long_lats, proj4string =CRS("+proj=longlat +datum=WGS84"))
  pp <- spTransform(points, "EPSG:3035")
  
  values <- extract(clc, points)
  
  df_long$clc_code <- values
  
  clc_classification <- tibble(clc_code = c(20, 30, 40, 50, 60, 70, 80, 90, 100, 111, 112, 113, 114, 115, 116, 121, 122, 123, 124, 125, 126, 200), 
                               classification = c("Shrubs", 
                                                  "Herbaceous_vegetation", 
                                                  "Cropland", 
                                                  "Urban", 
                                                  "Bare/sparse _vegetation", 
                                                  "Snow _and_Ice", 
                                                  "Permanent_water_bodies", 
                                                  "Herbaceous_wetland", 
                                                  "Moss_and_lichen",
                                                  "Closed_forest_evergreen_needle_leaf",
                                                  "Closed_forest_evergreen_broad_leaf",
                                                  "Closed_forest_decidouus_needle_leaf",
                                                  "Closed_forest_deciduous_broad_leaf",
                                                  "Closed_forest_mixed",
                                                  "Closed_forest_unknown",
                                                  "Open_forest_evergreen_needle_leaf",
                                                  "Open_forest_evergreen_broad_leaf",
                                                  "Open_forest_deciduous_needle_leaf",
                                                  "Open_forest_deciduous_broad_leaf",
                                                  "Open_forest_mixed",
                                                  "Open_forest_unknown",
                                                  "Open_sea"))
  
  
  traj_overland_landcover_long <- left_join(df_long, clc_classification, by = "clc_code")
  traj_overland_landcover_long
  
}


# Get trajectories
traj_df <- get_trajectory(lat = lat, lon = lon, duration = duration, days = days, daily_hours = daily_hours)
traj_df %>% trajectory_plot(show_hourly=T)

# Get elevation data on trajectory locations
df_long <- get_elevation(traj_df)

# Do some stats
overland_stats(df_long)



# Make tiles over europe, get information about the terrain at the trajectory locations using said tiles
traj_overland_landcover_long <- make_raster_extract_landcover(df_long =df_long)




