library(tidyverse)
library(readxl)
library(leaflet)

# Set folder for the year for which you want to compile excel files

orig_wd = getwd()

my_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Lake Monitoring/2025/HCTF grant reviews/Waterbody Sampling Tables"

setwd(my_folder)

# Find files in that folder.
file_paths = list.files(path = "./",
                        pattern = "xls[x]?",
                        full.names = T)

file_names = stringr::str_remove_all(file_paths,".*Sampling Tables/")
file_names = stringr::str_extract(file_names,"[A-Z]+")

# Read in those excel files.
excel_files = purrr::map(file_paths, ~ {
  # Read in data
  dt = try(suppressMessages(readxl::read_excel(.x, skip = 2, col_types = 'text')))
  # Drop anything inside parentheses (these were example values)
  names(dt) = stringr::str_remove_all(names(dt), " \\(.*")
  # Convert column names to coding-friendly 'snake_case'
  dt = dt |> purrr::set_names(snakecase::to_snake_case)
  # Correct some columns that have wacky names
  if('justification_for_sampling_sites_and_frequency' %in% names(dt)){
    dt = dt |>
      dplyr::rename(justification_for_sampling = justification_for_sampling_sites_and_frequency)
  }
  if('does_the_proposed_sampling_frequency_different_from_current_priority_list' %in% names(dt)){
    dt = dt |>
      dplyr::rename(does_the_proposed_sampling_frequency_differ_from_current_priority_list = does_the_proposed_sampling_frequency_different_from_current_priority_list)
  }
  dt
}, .progress = TRUE)

names(excel_files) = file_names

# Bind them together.
dat = dplyr::bind_rows(excel_files, .id = "Group")

# Convert longitude and latitude back to numeric.
dat = dat |>
  dplyr::filter(!is.na(waterbody_name)) |>
  # Correct some typos in longitude and latitude
  dplyr::mutate(longitude = stringr::str_remove(longitude,"^\\.{1}")) |>
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude))

# Clean up some waterbody names.
dat = dat |>
  dplyr::mutate(waterbody_name = dplyr::case_when(
    waterbody_name == "Bridge River - near confluence with the Fraser River" ~ "Bridge River",
    waterbody_name == "Christina Lake, BC" ~ "Christina Lake",
    waterbody_name == "Koocanusa Lake" ~ "Lake Koocanusa",
    waterbody_name == "Lower Fraser River" ~ "Fraser River",
    waterbody_name == "Lac La Hache" ~ "Lac la Hache",
    waterbody_name == "Norbury Lake" ~ "Norbury Lakes",
    waterbody_name == "Pend d'Oreille" ~ "Pend-d'Oreille River",
    T ~ waterbody_name
  ))

dat_sf = sf::st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326)

setwd(orig_wd)

sf::write_sf(dat_sf, 'data/2025_sampling_locations.gpkg')
