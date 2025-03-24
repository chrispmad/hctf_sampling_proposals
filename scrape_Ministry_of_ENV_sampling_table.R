library(rvest)

url = "https://www2.gov.bc.ca/gov/content/environment/research-monitoring-reporting/monitoring/lake-monitoring/bc-lake-monitoring-network/lake-monitoring-network-sites"

my_session = session(url = url)

page_tables = my_session |>
  rvest::html_table()

sampling_table = page_tables[[1]]

env_sampling_sf = sampling_table |>
  purrr::set_names(snakecase::to_snake_case) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) stringr::str_squish(stringr::str_replace_all(x, "(\n|\t)", " ")))) |>
  dplyr::mutate(lat = as.numeric(stringr::str_extract(site_information, "[0-9]{2}\\.[0-9]+(?= N)")),
                long = as.numeric(stringr::str_extract(site_information, "[0-9]{3}\\.[0-9]+(?= W)"))) |>
  dplyr::mutate(long = ifelse(long > 0, -1 * long, long)) |>
  sf::st_as_sf(coords = c("long","lat"), crs = 4326)

sf::write_sf(env_sampling_sf, "data/ministry_of_env_sampling.gpkg")
