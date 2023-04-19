install.packages("auk")
install.packages("dplyr")
install.packages("lubridate")
install.packages("sf")
install.packages("gridExtra")
install.packages ("tidyverse")

#Set Path
auk_set_ebd_path("~/Downloads/Kristen's Data/")

#Loading Packages#
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
<<<<<<< HEAD
library(dplyr)
=======

>>>>>>> 09be6019d2952c31cddeff6c1a05ed84c00d8927

#To avoid future problems#
select <- dplyr::select

<<<<<<< HEAD
#Set-up Data Directory# 
=======
#Set-up Data Directory 
>>>>>>> 09be6019d2952c31cddeff6c1a05ed84c00d8927
dir.create("data", showWarnings = FALSE)

ebd <- auk_ebd("ebd_CA-ON_bkcchi_199001_202301_relDec-2022.txt", 
               file_sampling = "ebd_sampling_relDec-2022.txt")

<<<<<<< HEAD
##Filtering the data#
=======
#Filtering the data#
>>>>>>> 09be6019d2952c31cddeff6c1a05ed84c00d8927
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
<<<<<<< HEAD
##Update it worked????##
=======
>>>>>>> 09be6019d2952c31cddeff6c1a05ed84c00d8927
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

<<<<<<< HEAD
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

# download bcrs
tmp_dir <- normalizePath(tempdir())
tmp_bcr <- file.path(tmp_dir, "bcr.zip")
paste0("https://www.birdscanada.org/research/gislab/download/", 
       "bcr_terrestrial_shape.zip") %>% 
  download.file(destfile = tmp_bcr)
unzip(tmp_bcr, exdir = tmp_dir)
bcr <- file.path(tmp_dir, "BCR_Terrestrial_master_International.shp") %>% 
  read_sf() %>% 
  select(bcr_code = BCR, bcr_name = LABEL) %>% 
  filter(bcr_code == 13)

  
#my file path#
bcr <- ("BCR_Terrestrial_master_International.shp")
read_sf() %>% 
  select(bcr_code = BCR, bcr_name = LABEL) %>% 
  filter(bcr_code == 13)
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

#Did not work#
read_sf
install.packages("sf")
library(sf)
#Trying Again#
st_transform(crs = "ESRI:102003")
map_proj <- st_crs(ESRI:102003)
ne_land <- read_sf("gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
bcr <- read_sf("data/gis-data.gpkg", "bcr") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

##Relative Abundance##
library(raster)
library(dggridR)
library(pdp)
library(mgcv)
library(fitdistrplus)
library(viridis)
library(fields)
# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection

# set random number seed to insure fully repeatable results
set.seed(1)

# setup output directory for saved results
if (!dir.exists("output")) {
  dir.create("output")
}

# ebird data
ebird <- read_csv("data/ebd_woothr_june_bcr27_zf.csv") %>% 
  mutate(protocol_type = factor(protocol_type, 
                                levels = c("Stationary" , "Traveling"))) %>%
  # remove observations with no count
  filter(!is.na(observation_count))

# modis habitat covariates
habitat <- read_csv("data/pland-elev_location-year.csv") %>% 
  mutate(year = as.integer(year))

# combine ebird and habitat data
ebird_habitat <- inner_join(ebird, habitat, by = c("locality_id", "year"))

# prediction surface
pred_surface <- read_csv("data/pland-elev_prediction-surface.csv")
# latest year of landcover data
max_lc_year <- pred_surface$year[1]
r <- raster("data/prediction-surface.tif")

# load gis data for making maps
map_proj <- st_crs(102003)
ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
bcr <- read_sf("data/gis-data.gpkg", "bcr") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
=======

###################################
ebd_dir <- "/Users/marleyabdull/Downloads/Kristen's Data/ebd_CA-ON_bkcchi_199001_202301_relDec-2022.txt"

# ebd
f <- file.path (ebd_dir, "ebd_CA-ON_bkcchi_199001_202301_relDec-2022.txt")
f_clean <- file.path (ebd_dir, "ebd_relbcchi_clean.txt")
auk_clean(f, f_out = f_clean, remove_text = TRUE)
# sampling
f_sampling <- file.path(ebd_dir, "ebd_sampling_relMay-2018.txt")
f_sampling_clean <- file.path(ebd_dir, "ebd_sampling_relDec-2022_clean.txt")
auk_clean(f, f_out = f_sampling_clean, remove_text = TRUE)

# define the paths to ebd and sampling event files
f_in_ebd <- file.path(ebd_dir, "ebd_relbcchi_clean.txt")
f_in_sampling <- file.path(ebd_dir, "ebd_sampling_relDec-2022_clean.txt")
# create an object referencing these files
auk_ebd(file = f_in_ebd, file_sampling = f_in_sampling)





>>>>>>> 09be6019d2952c31cddeff6c1a05ed84c00d8927
