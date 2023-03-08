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

#Filtering the data
auk_protocol(protocol = c("Stationary","Traveling")) %>%
auk_complete()
auk_filter()