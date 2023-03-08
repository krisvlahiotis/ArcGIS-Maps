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

#Set-up Data Directory 
ebd <- auk_ebd("ebd_CA-ON_bkcchi_199001_202301_relDec-2022.txt", 
               file_sampling = "ebd_sampling_relDec-2022.txt")

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
ebd_filters

#Output files
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
#Insert two new filtered files
f_ebd <- file.path(data_dir, "")
f_sampling <- file.path(data_dir, "")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

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

