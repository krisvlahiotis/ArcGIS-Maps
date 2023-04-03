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


#To avoid future problems#
select <- dplyr::select

#Set-up Data Directory 
dir.create("data", showWarnings = FALSE)

ebd <- auk_ebd("ebd_CA-ON_bkcchi_199001_202301_relDec-2022.txt", 
               file_sampling = "ebd_sampling_relDec-2022.txt")

#Filtering the data#
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





