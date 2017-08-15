library(tidyverse)
library(lubridate)

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

# ext_tracks <- read_fwf(file.path("G:","R projects", "hurricane_geom", "data", "ebtrk_atlc_1988_2015.txt"),
#                        fwf_widths(ext_tracks_widths, ext_tracks_colnames),
#                        na = "-99")
ext_tracks <- read_fwf(file.path("~", "R projects", "hurricane_geom", "data", "ebtrk_atlc_1988_2015.txt"),
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

ext_tracks %>% 
  select(storm_name, month, day, hour, year, latitude, longitude, starts_with("radius")) %>% 
  gather(sector_speed, radii, starts_with("radius"), na.rm = TRUE) %>% 
  separate(sector_speed, c("dumb", "wind_speed", "sector")) %>% 
  transmute(storm_id = paste(stringr::str_to_title(storm_name), year, sep = "-"), 
            date = ymd_h(paste(year, month, day, hour, sep = "-")),
            lattitude = latitude,
            longitude = -longitude, 
            wind_speed,
            sector,
            radii
             ) %>% 
  spread(sector, radii)
  #filter(storm_id == "Katrina-2005", date == ymd_hms("2005-08-29 12:00:00"))

#Target results         
##       storm_id                date latitude longitude wind_speed  ne  nw
## 1 Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         34 200 100
## 2 Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         50 120  75
## 3 Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         64  90  60
##    se  sw
## 1 200 150
## 2 120  75
## 3  90  60