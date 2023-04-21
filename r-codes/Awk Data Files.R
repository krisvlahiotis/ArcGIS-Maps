install.packages("auk")
install.packages("dplyr")
install.packages("lubridate")
install.packages("sf")
install.packages("gridExtra")
install.packages ("tidyverse")

#Loading Packages#
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(dplyr)

#To avoid future problems#
select <- dplyr::select

#Set-up Data Directory# 
dir.create("data", showWarnings = FALSE)

ebd <- auk_ebd("ebd_CA-ON_bkcchi_199001_202301_relDec-2022.txt", 
               file_sampling = "ebd_sampling_relDec-2022.txt")

##Filtering the data#
ebd_filters <- ebd %>% 
  auk_species("Black-capped Chickadee") %>% 
  # lower great lakes bcr
  auk_bcr(bcr = 13) %>% 
  # january 1993 to january 2023
  auk_date(date = c("1993-01-01", "2023-01-01")) %>% 
  # restrict to the standard traveling and stationary count protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete()
ebd_filters

##AWK Script not working, moving on##  
##Update it worked????##
# output files
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "ebd_bcchi_clean.txt")
f_sampling <- file.path(data_dir, "ebd_checklists_clean.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

####Zero Filling###
f_ebd <- ("ebd_bcchi_clean.txt")
f_sampling <-("ebd_checklists_clean.txt")

ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = TRUE)

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
ebd_zf <- ebd_zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )
##Variation in Dectectibility##
# additional filtering
ebd_zf_filtered <- ebd_zf %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # last 30 years of data
    year >= 1993,
    # 10 or fewer observers
    number_observers <= 10)

##Redundant Variables##
ebird <- ebd_zf_filtered %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, 
         duration_minutes, effort_distance_km,
         number_observers)

write_csv(ebird, "ebd_bcchi_zf.csv", na = "")

###GIS Data and Visualizations###
library(sf)
library(rnaturalearth)
library(dplyr)

# file to save spatial data
gpkg_dir <- "data"
if (!dir.exists(gpkg_dir)) {
  dir.create(gpkg_dir)
}
f_ne <- file.path(gpkg_dir, "gis-data.gpkg")

##Reading in shapefile##
bcr<-read_sf ("BCR_Terrestrial_master_International.shp")
view(bcr)

# filtering shape file to only select data from Southern Ontariobcr_SouthernOntario<- bcr %>%
bcr_SouthernOntario<- bcr %>%
  select(bcr_code = BCR, bcr_name = Label) %>% 
  filter(bcr_code == 13)  

# verifying that the data are representative of Southern Ontario
plot(bcr_SouthernOntario)

# clean up
list.files(tmp_dir, "bcr", ignore.case = TRUE, full.names = TRUE) %>% 
  unlink()

# political boundaries
# land border with lakes removed
ne_land <- ne_download(scale = 50, category = "cultural",
                       type = "admin_0_countries_lakes",
                       returnclass = "sf") %>%
  filter(CONTINENT == "North America") %>%
  st_set_precision(1e6) %>%
  st_union()
# country lines
# downloaded globally then filtered to north america with st_intersect()
ne_country_lines <- ne_download(scale = 50, category = "cultural",
                                type = "admin_0_boundary_lines_land",
                                returnclass = "sf") %>% 
  st_geometry()
ne_country_lines <- st_intersects(ne_country_lines, ne_land, sparse = FALSE) %>%
  as.logical() %>%
  {ne_country_lines[.]}
# states, north america
ne_state_lines <- ne_download(scale = 50, category = "cultural",
                              type = "admin_1_states_provinces_lines",
                              returnclass = "sf") %>%
  filter(adm0_a3 %in% c("USA", "CAN")) %>%
  mutate(iso_a2 = recode(adm0_a3, USA = "US", CAN = "CAN")) %>% 
  select(country = adm0_name, country_code = iso_a2)

# output
unlink(f_ne)
write_sf(ne_land, f_ne, "ne_land")
write_sf(ne_country_lines, f_ne, "ne_country_lines")
write_sf(ne_state_lines, f_ne, "ne_state_lines")
write_sf(bcr, f_ne, "bcr")

