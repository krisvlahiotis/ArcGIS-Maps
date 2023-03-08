#R Packages
install.packages("sf")
install.packages("remotes")
remotes::install_github("mstrimas/ebppackages")
install.packages("MODIS")
library(magrittr)
install.packages("magrittr")

#Set Path
auk::auk_set_ebd_path("~/ebird Data/")
auk::auk_set_ebd_path("C:/Users/kv8/Documents/Brock Biology/BIOL 4F91/GIS Data/ebird Data/")

#Installing auk
install.packages("auk")
library(auk)

#Bird Conservation Region - Lower Great Lakes
auk_bcr(bcr = 13)
  

#Filtering the data
ebd_filters <- ebd %>% 
auk_species("Black-capped Chickadee") %>% 
# lower great lakes bcr
auk_bcr(bcr = 13) %>% 
# january 1993 to january 2023
auk_date(date = c("1993-01-01", "2023-01-01")) %>% 
# restrict to the standard traveling and stationary count protocols
auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
auk_complete()

#GIS Layers
library(sf)
library(rnaturalearth)
library(dplyr)

# file to save spatial data
gpkg_dir <- "data"
if (!dir.exists(gpkg_dir)) {
  dir.create(gpkg_dir)
}
f_ne <- file.path(gpkg_dir, "gis-data.gpkg")

