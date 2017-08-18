library(tidyverse)
library(lubridate)
library(ggmap)

#' Read input data
#'
#' @param file_path Path of input file 
#'
#' @return Returns a tidy data frame
#'
#' @examples read_input_data(file.path("~", "R projects", "hurricane_geom", "data", "ebtrk_atlc_1988_2015.txt"))
read_input_data <- function(file_path) {
  #Read in input file
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
  
  
  ext_tracks <- read_fwf(file_path,
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")
  
  #Tidy input
  ext_tracks %>% 
    select(storm_name, month, day, hour, year, latitude, longitude, starts_with("radius")) %>% 
    gather(sector_speed, radii, starts_with("radius"), na.rm = TRUE) %>% 
    separate(sector_speed, c("dumb", "wind_speed", "sector")) %>% 
    transmute(storm_id = paste(stringr::str_to_title(storm_name), year, sep = "-"), 
              date = ymd_h(paste(year, month, day, hour, sep = "-")),
              latitude = latitude,
              longitude = -longitude, 
              wind_speed,
              sector,
              radii
               ) %>% 
    spread(sector, radii)
}

#' Stat ggproto object that returns hurricane radii data that could be plotted as a polygon
#' @param scale_radii Scaling factor to scale hurricane radii, defaults to 1.
StatHurricane <- ggproto("StatHurricane", Stat,
                     compute_group = function(data, scales, scale_radii) {
                       #unit conversion
                       nm_to_metres <- 1852
                       
                       ne <- geosphere::destPoint(c(data$x, data$y), b = 1:90, d = data$r_ne * nm_to_metres * scale_radii)
                       se <- geosphere::destPoint(c(data$x, data$y), b = 91:180, d = data$r_se * nm_to_metres * scale_radii)
                       sw <- geosphere::destPoint(c(data$x, data$y), b = 181:270, d = data$r_sw * nm_to_metres * scale_radii)
                       nw <- geosphere::destPoint(c(data$x, data$y), b = 271:360, d = data$r_nw * nm_to_metres * scale_radii)
                       
                       data <- data.frame(rbind(ne, se, sw, nw))
                       names(data)<-c("x","y")
                       data
                     },
                     
                     required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw")
)

#' stat_hurricane layer function
#' @param scale_radii Scaling factor to scale hurricane radii, defaults to 1.
stat_hurricane<- function(mapping = NULL, data = NULL, geom = "polygon",
                          position = "identity", show.legend = NA,
                          outliers = TRUE, inherit.aes = TRUE,
                          scale_radii = 1, ...) {
  
  ggplot2::layer(
    stat = StatHurricane,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(outliers = outliers, scale_radii = scale_radii, ...)
  )
}

#' Create Geom ggproto object
GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
                         required_aes = c("x", "y"),
                         default_aes = aes(fill = 'red', colour = 'red', size=0.5, alpha=1, linetype=1)
)


#' Layer function for geom_hurricane
#'
#' @inheritParams ggplot2::geom_polygon 
#' @param scale_radii Scaling factor for hurricane radius'
geom_hurricane <- function(mapping = NULL, data = NULL,stat="hurricane",
                           position = "identity", show.legend = NA,
                           na.rm = FALSE, inherit.aes = TRUE, scale_radii=1, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat=stat,
    geom = GeomHurricane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, scale_radii=scale_radii, ...)
  )
}

tidy_ext_tracks <- read_input_data(file.path("~", "R projects", "hurricane_geom", "data", "ebtrk_atlc_1988_2015.txt"))

katrina <- tidy_ext_tracks %>% 
  filter(storm_id == "Katrina-2005", date == ymd_hms("2005-08-29 12:00:00"))

#' Test plot
ggplot(data = katrina) +
  geom_hurricane(aes(x = longitude, y = latitude,
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) 

#' Extract hurricane ike single observation
ike <- tidy_ext_tracks %>% 
  filter(storm_id == "Ike-2008", date == ymd_hms("2008-09-13 12:00:00"))

#' Plot ike single observation over map
get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = ike,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
